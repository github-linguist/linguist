options indenting = 2

options no_unused_block_arguments = false
options no_unused_function_arguments = false
options multiple_contexts
options strict_smart_pointers = true

struct EntityId {
  id: uint;
}

let INVALID_ENTITY_ID = EntityId(id=0xFFFFFFFFu);

def operator == (a: EntityId; b: EntityId): bool {
  return a.id == b.id;
}

def operator != (a: EntityId; b: EntityId): bool {
  return a.id != b.id;
}

struct Position {
  x: float;
  y: float;
}

struct Velocity {
  dx: float;
  dy: float;
}

struct Health {
  current: int;
  max: int;
}

let MAX_ENTITIES = 10;
var entity_id_counter: uint = 0u;
var active_entities: int = 0;

var positions: array<Position>;
var velocities: array<Velocity>;
var healths: array<Health>;

var has_position: array<bool>;
var has_velocity: array<bool>;
var has_health: array<bool>;

def create_entity(): EntityId {
  if (active_entities < MAX_ENTITIES) {
    let new_id = entity_id_counter;
    entity_id_counter++;
    var eid = EntityId(id=new_id);
    let index = int(eid.id);

    if (index >= length(has_position)) {
      let new_size = index + 1;
      if (length(positions) < new_size) resize(positions, new_size);
      if (length(velocities) < new_size) resize(velocities, new_size);
      if (length(healths) < new_size) resize(healths, new_size);
      if (length(has_position) < new_size) resize(has_position, new_size);
      if (length(has_velocity) < new_size) resize(has_velocity, new_size);
      if (length(has_health) < new_size) resize(has_health, new_size);
    }

    has_position[index] = false;
    has_velocity[index] = false;
    has_health[index] = false;
    active_entities++;
    return eid;
  } else {
    return EntityId(id=INVALID_ENTITY_ID.id);
  }
}

def add_position_component(eid: EntityId; comp: Position) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    let index = int(eid.id);
    positions[index] = comp;
    has_position[index] = true;
  }
}

def add_velocity_component(eid: EntityId; comp: Velocity) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    let index = int(eid.id);
    velocities[index] = comp;
    has_velocity[index] = true;
  }
}

def add_health_component(eid: EntityId; comp: Health) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    let index = int(eid.id);
    healths[index] = comp;
    has_health[index] = true;
  }
}

def remove_position_component(eid: EntityId) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    has_position[int(eid.id)] = false;
  }
}

def remove_velocity_component(eid: EntityId) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    has_velocity[int(eid.id)] = false;
  }
}

def remove_health_component(eid: EntityId) {
  if (eid != INVALID_ENTITY_ID && eid.id < entity_id_counter) {
    has_health[int(eid.id)] = false;
  }
}

def movement_system(dt: float) {
  unsafe {
    for (i_idx in range(0, int(entity_id_counter))) {
      if (has_position[i_idx] && has_velocity[i_idx]) {
        var pos: Position& = positions[i_idx];
        let vel: Velocity& = velocities[i_idx];
        pos.x += vel.dx * dt;
        pos.y += vel.dy * dt;
      }
    }
  }
}

def damage_system(damage_amount: int) {
  unsafe {
    for (i_idx in range(0, int(entity_id_counter))) {
      if (has_health[i_idx] && has_velocity[i_idx]) {
        var hp: Health& = healths[i_idx];
        hp.current -= damage_amount;
        if (hp.current < 0) {
          hp.current = 0;
        }
      }
    }
  }
}

def cleanup_system() {
  unsafe {
    for (i_idx in range(0, int(entity_id_counter))) {
      if (has_health[i_idx]) {
        let hp: Health& = healths[i_idx];
        if (hp.current <= 0) {
          let current_eid = EntityId(id=uint(i_idx));
          print("Entity {current_eid.id} has run out of health. Removing components.\n");
          remove_position_component(current_eid);
          remove_velocity_component(current_eid);
          remove_health_component(current_eid);
        }
      }
    }
  }
}

def print_entity_status_system() {
  unsafe {
    for (i_idx in range(0, int(entity_id_counter))) {
      let current_eid = EntityId(id=uint(i_idx));
      var status_string = "Entity {current_eid.id}:";
      var has_any_component_for_this_id = false;

      if (i_idx < length(has_position) && has_position[i_idx]) {
        let pos: Position& = positions[i_idx];
        status_string += " Pos({pos.x},{pos.y})";
        has_any_component_for_this_id = true;
      }
      if (i_idx < length(has_velocity) && has_velocity[i_idx]) {
        let vel: Velocity& = velocities[i_idx];
        status_string += " Vel({vel.dx},{vel.dy})";
        has_any_component_for_this_id = true;
      }
      if (i_idx < length(has_health) && has_health[i_idx]) {
        let hp: Health& = healths[i_idx];
        status_string += " HP({hp.current}/{hp.max})";
        has_any_component_for_this_id = true;
      }

      if (has_any_component_for_this_id) {
        print("{status_string}\n");
      }
    }
  }
}

[export]
def main() {
  let e1 = create_entity();
  if (e1 != INVALID_ENTITY_ID) {
    add_position_component(e1, Position(x=0.0, y=0.0));
    add_velocity_component(e1, Velocity(dx=1.0, dy=0.5));
    add_health_component(e1, Health(current=100, max=100));
  }

  let e2 = create_entity();
  if (e2 != INVALID_ENTITY_ID) {
    add_position_component(e2, Position(x=10.0, y=5.0));
    add_health_component(e2, Health(current=50, max=50));
  }

  let e3 = create_entity();
  if (e3 != INVALID_ENTITY_ID) {
    add_position_component(e3, Position(x=-5.0, y=-5.0));
    add_velocity_component(e3, Velocity(dx=-0.2, dy=1.0));
    add_health_component(e3, Health(current=75, max=75));
  }

  let time_step = 1.0;
  let base_damage = 30;

  for (i_step in range(0, 5)) {
    print("--- Simulation Step {i_step} ---\n");
    movement_system(time_step);
    damage_system(base_damage);
    cleanup_system();
    print_entity_status_system();
    if (i_step == 1) {
      let e4 = create_entity();
      if (e4 != INVALID_ENTITY_ID) {
        print("Adding new entity e4 (id {e4.id}) at step {i_step}\n");
        add_position_component(e4, Position(x=2.0, y=2.0));
        add_velocity_component(e4, Velocity(dx=0.1, dy=-0.1));
        add_health_component(e4, Health(current=120, max=120));
      }
    }
  }
}