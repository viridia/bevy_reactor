use std::marker::PhantomData;

use bevy::{
    ecs::{
        component::Component,
        template::{Template, TemplateContext},
    },
    prelude::{Entity, EntityWorldMut, Result, World},
    scene2::{PatchContext, Scene},
};

use crate::{
    Cx, TrackingScope,
    cx::Lens,
    owner::OwnedBy,
    reaction::{InitialReactionCommand, Reaction, ReactionCell},
};

/// A reaction which runs an arbitrary computation. There are two phases: the deps phase, which
/// gathers reactive inputs, and the effect phase, which applies the computed deps value to
/// the target entity.
pub struct EffectReaction<D, Deps: Lens<D>, EffectFn: Fn(&mut EntityWorldMut, D)> {
    target: Entity,
    deps: Deps,
    effect: EffectFn,
    marker: PhantomData<D>,
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> EffectReaction<D, Deps, EffectFn>
{
    fn apply(&self, _owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        let cx = Cx::new(world, self.target, tracking);
        let val = self.deps.call(&cx);

        let mut target = world.entity_mut(self.target);
        (self.effect)(&mut target, val);
    }
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> Reaction for EffectReaction<D, Deps, EffectFn>
{
    fn react(&mut self, owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        self.apply(owner, world, tracking);
    }
}

/// Scene element for creating [`effect`] reactions.
#[derive(Clone)]
pub struct Effect<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> {
    deps: Deps,
    effect: EffectFn,
    marker: std::marker::PhantomData<D>,
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> Effect<D, Deps, EffectFn>
{
    pub fn new(deps: Deps, effect_fn: EffectFn) -> Self {
        Self {
            deps,
            effect: effect_fn,
            marker: std::marker::PhantomData,
        }
    }
}

impl<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
> Template for Effect<D, Deps, EffectFn>
{
    type Output = ();

    fn build(&mut self, target: &mut TemplateContext) -> Result<Self::Output> {
        let target_id = target.entity.id();
        target.entity.world_scope(|world| {
            // Create the reaction
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .spawn((
                    ReactionCell::new(EffectReaction {
                        target: target_id,
                        deps: self.deps.clone(),
                        effect: self.effect.clone(),
                        marker: PhantomData,
                    }),
                    scope,
                ))
                .id();

            // Add the reaction to the target entity
            world
                .entity_mut(target_id)
                .add_one_related::<OwnedBy>(reaction);

            // Set up to run the reaction after creation.
            world.commands().queue(InitialReactionCommand(reaction));
        });
        Ok(())
    }
}

impl<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
> Scene for Effect<D, Deps, EffectFn>
{
    fn patch(&self, _context: &mut PatchContext, scene: &mut bevy::scene2::ResolvedScene) {
        scene.push_template(Effect {
            deps: self.deps.clone(),
            effect: self.effect.clone(),
            marker: std::marker::PhantomData,
        });
    }
}

pub fn effect<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
>(
    deps: Deps,
    effect: EffectFn,
) -> impl Scene {
    Effect {
        deps,
        effect,
        marker: std::marker::PhantomData,
    }
}

/// A reaction which inserts a dynamic component into the target entity.
pub struct InsertDynReaction<D, Deps: Lens<D>, C: Component, Factory: Fn(D) -> C> {
    target: Entity,
    deps: Deps,
    factory: Factory,
    marker: PhantomData<D>,
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Send + Sync + 'static,
> InsertDynReaction<D, Deps, C, Factory>
{
    fn apply(&self, _owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        let cx = Cx::new(world, self.target, tracking);
        let val = self.deps.call(&cx);

        let mut target = world.entity_mut(self.target);
        target.insert((self.factory)(val));
    }
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Send + Sync + 'static,
> Reaction for InsertDynReaction<D, Deps, C, Factory>
{
    fn react(&mut self, owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        self.apply(owner, world, tracking);
    }
}

/// Scene element for creating [`insert_when`] reactions.
#[derive(Clone)]
pub struct InsertDyn<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Send + Sync + 'static,
> {
    deps: Deps,
    factory: Factory,
    marker: std::marker::PhantomData<D>,
}

impl<
    D,
    Deps: Lens<D> + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Send + Sync + 'static,
> InsertDyn<D, Deps, C, Factory>
{
    pub fn new(deps: Deps, factory: Factory) -> Self {
        Self {
            deps,
            factory,
            marker: std::marker::PhantomData,
        }
    }
}

impl<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Clone + Send + Sync + 'static,
> Template for InsertDyn<D, Deps, C, Factory>
{
    type Output = ();

    fn build(&mut self, target: &mut TemplateContext) -> Result<Self::Output> {
        let target_id = target.entity.id();
        target.entity.world_scope(|world| {
            // Create the reaction
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .spawn((
                    ReactionCell::new(InsertDynReaction {
                        target: target_id,
                        deps: self.deps.clone(),
                        factory: self.factory.clone(),
                        marker: PhantomData,
                    }),
                    scope,
                ))
                .id();

            // Add the reaction to the target entity
            world
                .entity_mut(target_id)
                .add_one_related::<OwnedBy>(reaction);

            // Set up to run the reaction after creation.
            world.commands().queue(InitialReactionCommand(reaction));
        });
        Ok(())
    }
}

impl<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Clone + Send + Sync + 'static,
> Scene for InsertDyn<D, Deps, C, Factory>
{
    fn patch(&self, _context: &mut PatchContext, scene: &mut bevy::scene2::ResolvedScene) {
        scene.push_template(InsertDyn {
            deps: self.deps.clone(),
            factory: self.factory.clone(),
            marker: std::marker::PhantomData,
        });
    }
}

pub fn insert_dyn<
    D: Send + Sync + 'static,
    Deps: Lens<D> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn(D) -> C + Clone + Send + Sync + 'static,
>(
    deps: Deps,
    factory: Factory,
) -> impl Scene {
    InsertDyn {
        deps,
        factory,
        marker: std::marker::PhantomData,
    }
}

/// A reaction which inserts or removes a component from a target entity whenever the condition
/// changes.
pub struct InsertWhenReaction<Condition: Lens<bool>, C: Component, Factory: Fn() -> C> {
    target: Entity,
    condition: Condition,
    factory: Factory,
}

impl<
    Condition: Lens<bool> + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Send + Sync + 'static,
> InsertWhenReaction<Condition, C, Factory>
{
    fn apply(&self, _owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        let cx = Cx::new(world, self.target, tracking);
        let cond = self.condition.call(&cx);

        let mut target = world.entity_mut(self.target);
        if cond {
            target.insert((self.factory)());
        } else {
            target.remove::<C>();
        }
    }
}

impl<
    Condition: Lens<bool> + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Send + Sync + 'static,
> Reaction for InsertWhenReaction<Condition, C, Factory>
{
    fn react(&mut self, owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        self.apply(owner, world, tracking);
    }
}

/// Scene element for creating [`insert_when`] reactions.
#[derive(Clone)]
pub struct InsertWhen<
    Condition: Lens<bool> + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Send + Sync + 'static,
> {
    condition: Condition,
    factory: Factory,
}

impl<
    Condition: Lens<bool> + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Send + Sync + 'static,
> InsertWhen<Condition, C, Factory>
{
    pub fn new(condition: Condition, factory: Factory) -> Self {
        Self { condition, factory }
    }
}

impl<
    Condition: Lens<bool> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Clone + Send + Sync + 'static,
> Template for InsertWhen<Condition, C, Factory>
{
    type Output = ();

    fn build(&mut self, target: &mut TemplateContext) -> Result<Self::Output> {
        let target_id = target.entity.id();
        target.entity.world_scope(|world| {
            // Create the reaction
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .spawn((
                    ReactionCell::new(InsertWhenReaction {
                        target: target_id,
                        condition: self.condition.clone(),
                        factory: self.factory.clone(),
                        // marker: PhantomData,
                    }),
                    scope,
                ))
                .id();

            // Add the reaction to the target entity
            world
                .entity_mut(target_id)
                .add_one_related::<OwnedBy>(reaction);

            // Set up to run the reaction after creation.
            world.commands().queue(InitialReactionCommand(reaction));
        });
        Ok(())
    }
}

impl<
    Condition: Lens<bool> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Clone + Send + Sync + 'static,
> Scene for InsertWhen<Condition, C, Factory>
{
    fn patch(&self, _context: &mut PatchContext, scene: &mut bevy::scene2::ResolvedScene) {
        scene.push_template(InsertWhen {
            condition: self.condition.clone(),
            factory: self.factory.clone(),
        });
    }
}

pub fn insert_when<
    Condition: Lens<bool> + Clone + Send + Sync + 'static,
    C: Component,
    Factory: Fn() -> C + Clone + Send + Sync + 'static,
>(
    condition: Condition,
    factory: Factory,
) -> impl Scene {
    InsertWhen { condition, factory }
}
