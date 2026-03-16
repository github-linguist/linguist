#define TRY_SET_CLASSNAME(ent, spawn_fn, spawn_fn_sym)\
    do {\
        if (spawn_fn == spawn_fn_sym) {\
            ent.classname = #spawn_fn_sym;\
        }\
    } while (FALSE)
    
void() monster_army, monster_boss, monster_demon1, monster_dog,
    monster_enforcer, monster_fish, monster_hell_knight, monster_knight,
    monster_ogre, monster_ogre_marksman, monster_oldone, monster_shalrath,
    monster_shambler, monster_tarbaby, monster_wizard,  monster_zombie;

void(vector, entity) spawn_tdeath;
void(vector) spawn_tfog;
void() FoundTarget;
float() droptofloor;

const float SPAWNER_FLAG_NO_TELEFLASH = 8;
const float SPAWNER_FLAG_ANGRY = 16;

void(entity monster) SpawnerSetVelocity = {
    // mimic behavior of monsterjump
    makevectors(self.angles);
    v_forward_z = 0;
    monster.velocity = v_forward * self.speed;
    monster.velocity_z = self.height;
};

void() SpawnerUse = {
    vector l_mins, l_maxs;
    entity spawner;

    entity monster = self.enemy;
    self.enemy = monster.owner;
    monster.owner = world;

    if (monster.think != monster_boss) {
        monster.movetype = MOVETYPE_STEP;
        monster.solid = SOLID_SLIDEBOX;
    }

    l_mins = monster.mins;
    l_maxs = monster.maxs;
    setmodel(monster, self.model);
    setsize(monster, l_mins, l_maxs);

    monster.health = self.max_health;
    monster.targetname = self.netname;

    spawner = self;
    self = monster;

    if (self.think) {
        self.think();
    } else {
        dprint("Spawned monster has no think!\n");
    }

    self = spawner;

    // un-droptofloor
    monster.origin = self.origin + '0 0 1';
    
    if (monster.flags & FL_ONGROUND) {
        monster.flags-= FL_ONGROUND;
    }

    SpawnerSetVelocity(monster);

    if (!(self.spawnflags & SPAWNER_FLAG_NO_TELEFLASH)) {
        spawn_tfog(monster.origin);
    }

    spawn_tdeath(monster.origin, monster);

    if (monster.health > 0) {
        if (self.spawnflags & SPAWNER_FLAG_ANGRY) {
            if (activator.classname == "player") {
                monster.enemy = activator;
            } else {
                self = monster;
                monster.enemy = checkclient();
                self = spawner;

                if (!monster.enemy) {
                    monster.enemy = find(world, classname, "player");
                }
            }

            if (monster.enemy.flags & FL_NOTARGET) {
                monster.enemy = world;
            }

            if (monster.enemy) {
                monster.think = FoundTarget;
                monster.nextthink = time + 0.01;
            }
        }
    }

    if (!self.enemy) {
        self.use = SUB_Null;
    }
};

static void(entity monster, void() spawn_fn) SpawnerSetMonsterClassname = {
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_army);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_boss);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_demon1);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_dog);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_enforcer);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_fish);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_hell_knight);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_knight);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_ogre);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_ogre_marksman);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_oldone);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_shalrath);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_shambler);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_tarbaby);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_wizard);
    TRY_SET_CLASSNAME(monster, spawn_fn, monster_zombie);

    if (monster.classname == "") {
        dprint("Could not find classname for spawned monster\n");
    }
};

void() monster_spawner = {
    entity spawner;

    if (deathmatch) {
        remove(self);
        return;
    }

    if (self.count < 1) {
        self.count = 1;
    }

    // setup monster
    spawner = self;

    for (; spawner.count > 0; spawner.count--) {
        self = spawn();

        self.origin = spawner.origin;
        self.angles = spawner.angles;
        self.target = spawner.target;
        self.target2 = spawner.target2;
        self.squad = spawner.squad;
        self.io_send = spawner.io_send;
        self.delay = spawner.delay;
        self.spawnflags = spawner.spawnflags;

        if (self.spawnflags & SPAWNER_FLAG_NO_TELEFLASH) {
            self.spawnflags-= SPAWNER_FLAG_NO_TELEFLASH;
        }

        if (self.spawnflags & SPAWNER_FLAG_ANGRY) {
            self.spawnflags-= SPAWNER_FLAG_ANGRY;
        }

        SpawnerSetMonsterClassname(self, spawner.think);

        // spawn function
        spawner.think();

        self.nextthink = 0;

        // cache model (don't use setmodel)
        spawner.model = self.model;
        self.modelindex = 0;
        self.solid = SOLID_NOT;
        self.movetype = MOVETYPE_NONE;
        spawner.max_health = self.health;
        self.health = 0;
        self.owner = spawner.enemy;
        spawner.enemy = self;
    }

    self = spawner;
    self.use = SpawnerUse;
};
