# Graph

- Change cursor over current terminal
  - Add
  - Not allowed
  - EntityCursor needs a global override
- Integrate UiScale.
- Experiment with scaling

# Formulae

- The hard part is making the VM relatively efficient. The problem is that reading and writing fields
  of fields of structs requires keeping references to those fields, which raises issues around
  lifetimes.

- Examples of conditions:
  - `self.alive`
  - `self.hasInventoryItems` or `self.inventory.is_empty()`
  - `enemies().length > 0`
  - `!self.hasThreat && self.position.distanceTo(self.homePosition) < 2`
  - `!self.hasThreat && proximityToPlayer() && self.position.distanceTo(self.homePosition) < 3`
  - `!props.locked && !props.open && self.hasInventoryItems`
  - `isHourBetween(11, 7)`
  - `self.isTargetedByPlayer`
  - `props.following && enemies().length > 0`
  - `props.timer < 0`
  - ```
      when: (self, props) => {
        const [, setter] = self.findReadWriteSignal('open', plainBooleanType);
        return Boolean(setter);
      },
    ```
  - ```
      export function canAscend() {
        const { engine } = instanceScriptContext;
        const self = withSelf();
        if (isWallTile(self) && engine) {
          const player = engine.actors.player;
          if (player) {
            const height = tileHeight(self);
            return player.position.y < self.position.y + height / 2;
          }
        }
        return false;
      }
    ```
  - ```
      when: () =>
        Boolean(
          !self.hasThreat &&
            proximityToPlayer() &&
            !engine.quests.isInStage([['confront', null]], QUEST_ID)
        ),
    ```
  - `dest().hasAspect(builtin.waymark)`

# Grouping these:

- simple properties of the actor:
  - open (ContainerOpen marker present)
  - locked (ContainerLocked marker present)
  - following
- derived properties of the actor
  - alive (health above zero)
    - Health component .0 > 0
  - has threat (threat list not empty)
  - has enemies (filtered threat list by position and reduced reactivity)
  - has inventory items
  - distance to home position above threshold
- combination of actor and player properties
  - distance to player above threshold
  - vertical distance of player relative to self (stairs)
  - is targeted by player
- world properties
  - current time of day / date
  - current quest stage for quest ID
- more advanced:
  - whether a door has an input signal connected to it
  - timers

## Formula TODO

Next:

- lifetime analysis for locals (hard!)
  - can this be on a per-block level rather than per-instruction? I think so.
  - that is, for each block we want to know which locals are read and which are written
    - we can then determine which locals are no longer needed by looking at preds
- complex expressions
  - e.g. if { 0 } { 1 } else { 2 }
- error cases in build_exprs and gencode:
  - calling a static function non-statically
  - calling a non-static function statically
  - calling a non-function
  - attempting to use a function or type name as an lvalue

# New locals design:

- We'll need to separate params from locals, at least initially
- locals is a flat buffer of u8
- instructions:

  - OP_LOAD_LOCAL_BOOL (u16 stack offset)
  - OP_LOAD_LOCAL_I32 (u16)
  - OP_LOAD_LOCAL_I64 (u16)
  - OP_LOAD_LOCAL_F32 (u16)
  - OP_LOAD_LOCAL_F64 (u16)
  - OP_LOAD_LOCAL_STRING (u16)
  - OP_LOAD_LOCAL_ENTITY (u16)
  - OP_LOAD_LOCAL_REFLECT (u16) -> points to box<dyn PartialReflect>
  - similar instructions for OP_STORE...
  - box dropping:
    - the function object requires a table of offsets of boxes to be dropped.
    - this needs to be part of the call frame.
    - it consists of a &'static [u16]
    - what about dropping of string locals?
      - Maybe what we need is an explicit OP_DROP_XXX opcode.
        - OP_DROP_STRING
        - OP_DROP_REFLECT
      - This means doing some kind of lifetime analysis.
        - this would be easier if we had SSA values of some kind, but I don't know how to do that.
  - We also need to store the local size in the function
  - This means, then, that we can can represent params via a pointer to the locals of the
    previous call frame, so long as we align and offset each parameter consistently.

- optimize basic blocks:
  - jump to a block with only a terminator replaces that terminator.
  - jump to a block with only one predecessor can merge blocks.
  - block with no predecessor can be eliminated.
  - jump to a block that only pushes a bool can be reduced to a bool
  - note all of the above require examples that generate these patterns.

## Formula tasks

- type cast
- inferred function adaptors
- operators
  - shl / shr
  - log and/not
  - unary
- type alias
  - in script
  - from host
- control flow
  - if [done]
  - for
  - loop
  - return
  - break
  - continue
  - match? (pattern matching is hard, and requires tuples and other things)
- basic blocks
- complex types
  - string (more methods)
  - array (native type?)
  - struct
  - tuple
  - note: Vec is going to be hard to add methods to because of generics.
- import statements
  - parse
  - resolve
  - backend
- record max stack size in function
