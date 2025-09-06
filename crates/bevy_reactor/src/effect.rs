use bevy::{
    asset::{AssetServer, Assets},
    ecs::template::Template,
    prelude::{Entity, EntityWorldMut, Result, World},
    scene2::Scene,
};

use crate::{
    Cx, TrackingScope,
    owner::OwnedBy,
    reaction::{InitialReactionCommand, Reaction, ReactionCell},
};

/// A reaction which runs an arbitrary computation. There are two phases: the deps phase, which
/// gathers reactive inputs, and the effect phase, which applies the computed deps value to
/// the target entity.
/// TODO: we want to be able to pass a signal instance as well as a function here.
pub struct EffectCell<D, VF: Fn(&Cx) -> D, SF: Fn(&mut EntityWorldMut, D)> {
    target: Entity,
    deps_fn: VF,
    effect_fn: SF,
}

impl<
    D,
    DepsFn: Fn(&Cx) -> D + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> EffectCell<D, DepsFn, EffectFn>
{
    fn apply(&self, _owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        let cx = Cx::new(world, self.target, tracking);
        let val = (self.deps_fn)(&cx);

        let mut target = world.entity_mut(self.target);
        (self.effect_fn)(&mut target, val);
    }
}

impl<
    D,
    DepsFn: Fn(&Cx) -> D + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> Reaction for EffectCell<D, DepsFn, EffectFn>
{
    fn react(&mut self, owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        self.apply(owner, world, tracking);
    }
}

/// Scene element for creating [`Effect`] reactions.
#[derive(Clone)]
pub struct Effect<
    D,
    DepsFn: Fn(&Cx) -> D + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> {
    deps_fn: DepsFn,
    effect_fn: EffectFn,
    marker: std::marker::PhantomData<D>,
}

impl<
    D,
    DepsFn: Fn(&Cx) -> D + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Send + Sync + 'static,
> Effect<D, DepsFn, EffectFn>
{
    pub fn new(deps_fn: DepsFn, effect_fn: EffectFn) -> Self {
        Self {
            deps_fn,
            effect_fn,
            marker: std::marker::PhantomData,
        }
    }
}

impl<
    D: 'static,
    DepsFn: Fn(&Cx) -> D + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
> Template for Effect<D, DepsFn, EffectFn>
{
    type Output = ();

    fn build(&mut self, target: &mut EntityWorldMut) -> Result<Self::Output> {
        let target_id = target.id();
        target.world_scope(|world| {
            // Create the reaction
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .spawn((
                    ReactionCell::new(EffectCell {
                        target: target_id,
                        deps_fn: self.deps_fn.clone(),
                        effect_fn: self.effect_fn.clone(),
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
    DepsFn: Fn(&Cx) -> D + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
> Scene for Effect<D, DepsFn, EffectFn>
{
    fn patch(
        &self,
        _assets: &AssetServer,
        _patches: &Assets<bevy::scene2::ScenePatch>,
        scene: &mut bevy::scene2::ResolvedScene,
    ) {
        scene.push_template(Effect {
            deps_fn: self.deps_fn.clone(),
            effect_fn: self.effect_fn.clone(),
            marker: std::marker::PhantomData,
        });
    }
}

pub fn effect<
    D: Send + Sync + 'static,
    DepsFn: Fn(&Cx) -> D + Clone + Send + Sync + 'static,
    EffectFn: Fn(&mut EntityWorldMut, D) + Clone + Send + Sync + 'static,
>(
    deps_fn: DepsFn,
    effect_fn: EffectFn,
) -> impl Scene {
    Effect {
        deps_fn,
        effect_fn,
        marker: std::marker::PhantomData,
    }
}
