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

- if
- load module from asset

  - extension: .fmod

- function definitions
  - native
  - host
  - entity
- inferred function adaptors
- local vars
- parameters
  - script functions
  - native functions
  - entity methods
- operators
  - shl / shr
  - log and/not
  - unary
- type alias
- control flow
  - if
  - for
  - loop
  - return
  - break
  - continue
- bblocks
- complex types
  - string
  - array
  - struct
  - tuple
- import statements
  - parse
  - resolve
  - backend

[
:00 21, 0, 0, 0, // OP_LOAD_GLOBAL
:04 0, 0, 0, 0, // immediate
:08 22, 0, 0, 0, // OP_LOAD_ENTITY_PROP
:12 0, 0, 0, 0, // immediate
:16 5, 0, 0, 0, // OP_CONST_F32
:20 0, 0, 0, 0, // immediate f32
:24 168, // OP_GT_F32
:25 63, 0, 0, // OP_BRANCH_IF_FALSE
:28 16, 0, 0, 0, // immediate (16)
:32 5, 0, 0, 0, // OP_CONST_F32
:36 0, 0, 0, 64, // immediate
:40 62, 0, 0, 0, // OP_BRANCH
:44 8, 0, 0, 0,
:48 5, 0, 0, 0,
:52 0, 0, 64, 64,
:56 61 // OP_RET
