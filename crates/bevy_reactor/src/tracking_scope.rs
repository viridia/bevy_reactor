use std::sync::atomic::AtomicBool;

use bevy::{
    ecs::{
        component::{ComponentId, Tick},
        world::DeferredWorld,
    },
    platform::collections::HashSet,
    prelude::*,
};

use crate::reaction::ReactionCell;

/// A component that tracks the dependencies of a reactive task.
#[derive(Component)]
pub struct TrackingScope {
    /// Set of components that we are currently subscribed to.
    component_deps: HashSet<(Entity, ComponentId)>,

    /// Set of resources that we are currently subscribed to.
    resource_deps: HashSet<ComponentId>,

    /// Allows a tracking scope to be explictly marked as changed for reasons other than
    /// a component or resource dependency mutation.
    changed: AtomicBool,

    /// Engine tick used for determining if components have changed. This represents the
    /// time of the previous reaction.
    pub(crate) tick: Tick,

    /// List of cleanup functions to call when the scope is dropped.
    /// TODO: This is a concept taken from Solid, but I don't actually use it anywhere.
    /// The envisioned use case is for effects that need to undo the changes of the previous action
    /// (like stopping a timer or unsubscibing to a listener) before performing the next action.
    #[allow(clippy::type_complexity)]
    pub(crate) cleanups: Vec<Box<dyn FnOnce(&mut DeferredWorld) + 'static + Sync + Send>>,
}

/// A resource which, if inserted, displays the view entities that have reacted this frame.
#[derive(Resource)]
pub struct TrackingScopeTracing(pub Vec<Entity>);

impl FromWorld for TrackingScopeTracing {
    fn from_world(_world: &mut World) -> Self {
        Self(Vec::new())
    }
}

impl TrackingScope {
    /// Create a new tracking scope.
    pub fn new(tick: Tick) -> Self {
        Self {
            component_deps: HashSet::default(),
            resource_deps: HashSet::default(),
            changed: AtomicBool::new(false),
            tick,
            cleanups: Vec::new(),
        }
    }

    /// Add a cleanup function which will be run once before the next reaction.
    pub fn add_cleanup(
        &mut self,
        cleanup: impl FnOnce(&mut DeferredWorld) + 'static + Sync + Send,
    ) {
        self.cleanups.push(Box::new(cleanup));
    }

    /// Convenience method for adding a resource dependency.
    pub fn track_resource<T: Resource>(&mut self, world: &World) {
        self.resource_deps.insert(
            world
                .components()
                .resource_id::<T>()
                .expect("Unknown resource type"),
        );
    }

    /// Convenience method for adding a component dependency.
    pub(crate) fn track_component<C: Component>(&mut self, entity: Entity, world: &World) {
        self.track_component_id(
            entity,
            world
                .components()
                .component_id::<C>()
                .expect("Unknown component type"),
        );
    }

    /// Convenience method for adding a component dependency by component id.
    pub(crate) fn track_component_id(&mut self, entity: Entity, component: ComponentId) {
        self.component_deps.insert((entity, component));
    }

    /// Mark the scope as changed for reasons other than a component or resource dependency.
    pub fn set_changed(&self) {
        self.changed
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }

    /// Returns true if any of the dependencies of this scope have been updated since
    /// the previous reaction.
    pub fn dependencies_changed(&self, world: &World, tick: Tick) -> bool {
        self.components_changed(world, tick)
            || self.resources_changed(world, tick)
            || self.changed.load(std::sync::atomic::Ordering::Relaxed)
    }

    fn components_changed(&self, world: &World, tick: Tick) -> bool {
        self.component_deps.iter().any(|(e, c)| {
            world.get_entity(*e).is_ok_and(|e| {
                e.get_change_ticks_by_id(*c)
                    .map(|ct| ct.is_changed(self.tick, tick))
                    .unwrap_or(false)
            })
        })
    }

    fn resources_changed(&self, world: &World, tick: Tick) -> bool {
        self.resource_deps.iter().any(|c| {
            world
                .get_resource_change_ticks_by_id(*c)
                .map(|ct| ct.is_changed(self.tick, tick))
                .unwrap_or(false)
        })
    }

    /// Take the dependencies from another scope. Typically the other scope is a temporary
    /// scope that is used to compute the next set of dependencies.
    pub fn take_deps(&mut self, other: &mut Self) {
        self.component_deps = std::mem::take(&mut other.component_deps);
        self.resource_deps = std::mem::take(&mut other.resource_deps);
        self.cleanups = std::mem::take(&mut other.cleanups);
        self.changed = AtomicBool::new(other.changed.load(std::sync::atomic::Ordering::Relaxed));
    }
}

/// Component hook which runs the cleanups when a tracking scope is despawned.
pub(crate) fn cleanup_tracking_scopes(world: &mut World) {
    world
        .register_component_hooks::<TrackingScope>()
        .on_remove(|mut world, hook| {
            let mut scope = world.get_mut::<TrackingScope>(hook.entity).unwrap();
            let mut cleanups = std::mem::take(&mut scope.cleanups);
            for cleanup_fn in cleanups.drain(..) {
                cleanup_fn(&mut world);
            }
        });
}

fn run_cleanups(world: &mut World, changed: &[Entity]) {
    let mut deferred = DeferredWorld::from(world);
    for scope_entity in changed.iter() {
        let Some(mut scope) = deferred.get_mut::<TrackingScope>(*scope_entity) else {
            continue;
        };
        let mut cleanups = std::mem::take(&mut scope.cleanups);
        for cleanup_fn in cleanups.drain(..) {
            cleanup_fn(&mut deferred);
        }
    }
}

const MAX_DIVERGENCE_CT: usize = 32;

/// Run reactions whose dependencies have changed. This uses a "run to convergence" strategy:
/// running a reaction may trigger other reactions, so we loop until there are no more reactions
/// left to run. However, to avoid an infinite loop we require that the reactions eventually
/// reach a quiescent state. We count the number of "divergences" (cycles where the number
/// of reactions didn't decrease) and impose a strict limit on the number of such cycles.
pub(crate) fn run_reactions(world: &mut World) {
    let is_tracing = world.get_resource_mut::<TrackingScopeTracing>().is_some();
    let mut all_reactions: Vec<Entity> = Vec::new();
    let mut iteration_ct: usize = 0;
    let mut divergence_ct: usize = 0;
    let mut prev_change_ct: usize = 0;

    loop {
        // If this is the first iteration, use the world's change tick, otherwise bump the
        // tick and use that.
        let this_run = if iteration_ct > 0 {
            world.increment_change_tick()
        } else {
            world.change_tick()
        };

        // Find all tracking scopes that have changes.
        let mut scopes = world.query::<(Entity, &mut TrackingScope, &ReactionCell)>();
        let mut changed: Vec<Entity> = Vec::with_capacity(64);
        for (entity, scope, _) in scopes.iter(world) {
            if scope.dependencies_changed(world, this_run) {
                changed.push(entity);
            }
        }

        // Quit if there are no changes.
        if changed.is_empty() {
            break;
        }

        // In debug mode, record the changed reactions in a resource.
        if is_tracing {
            all_reactions.extend(changed.clone());
        }

        // Run any registered cleanup functions.
        run_cleanups(world, &changed);

        // Run reactions
        for scope_entity in changed.iter() {
            // Run the reaction
            if world.get_entity(*scope_entity).is_err() {
                continue;
            }
            let Some(cell) = world.entity(*scope_entity).get::<ReactionCell>() else {
                continue;
            };
            let mut next_scope = TrackingScope::new(this_run);
            let inner = cell.0.clone();
            let mut lock = inner.lock().unwrap();
            lock.react(*scope_entity, world, &mut next_scope);

            // Replace deps and cleanups in the current scope with the next scope.
            let (_, mut scope, _) = scopes.get_mut(world, *scope_entity).unwrap();
            scope.take_deps(&mut next_scope);
            scope.tick = this_run;
        }

        // Check for divergence.
        iteration_ct += 1;
        let change_ct = changed.len();
        if change_ct >= prev_change_ct {
            divergence_ct += 1;
            if divergence_ct > MAX_DIVERGENCE_CT {
                panic!("Reactions failed to converge, num changes: {change_ct}");
            }
        }
        prev_change_ct = change_ct;
    }

    // Record the changed entities for diagnostic purposes.
    if let Some(mut tracing) = world.get_resource_mut::<TrackingScopeTracing>() {
        std::mem::swap(&mut tracing.0, &mut all_reactions);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Resource, Default)]
    struct TestResource(bool);

    #[test]
    fn test_resource_deps_changed() {
        let mut world = World::default();
        let tick = world.change_tick();
        let mut scope = TrackingScope::new(tick);

        // No dependencies, so the result should be false
        assert!(!scope.dependencies_changed(&world, tick));

        world.increment_change_tick();
        world.insert_resource(TestResource(false));
        scope.track_resource::<TestResource>(&world);
        assert!(scope.resource_deps.len() == 1);

        // Resource added
        let tick = world.change_tick();
        assert!(scope.dependencies_changed(&world, tick));

        // Reset scope tick
        scope.tick = tick;
        assert!(!scope.dependencies_changed(&world, tick));

        // Mutate the resource
        world.increment_change_tick();
        world.get_resource_mut::<TestResource>().unwrap().0 = true;
        let tick = world.change_tick();
        assert!(scope.dependencies_changed(&world, tick));
    }
}
