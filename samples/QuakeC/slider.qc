float SLIDER_SPAWNFLAG_SOLID = 1;

float SLIDER_STATE_STOPPED = 0;
float SLIDER_STATE_FORWARD = 1;
float SLIDER_STATE_REVERSE = 2;

float SLIDER_BLOCKCHECK_DTIME = 0.02;
float SLIDER_DMG_DTIME = 0.3;

// float SLIDER_DMGSTYLE_GROW = 0; (DEFAULT)
float SLIDER_DMGSTYLE_CONST = 1;
float SLIDER_DMGSTYLE_CRUSH = 2;

void() SliderStop = {
    self.state = SLIDER_STATE_STOPPED;
    self.velocity = '0 0 0';
    self.cnt = self.ltime;

    sound(self, CHAN_VOICE, self.noise1, 1, ATTN_NORM);
};

void() SliderStopAtLanding = {
    self.io_send = self.io_received;

    // Restore cached activator if possible; otherwise self
    if (self.enemy) {
        activator = self.enemy;
        self.enemy = world;
    } else {
        activator = self;
    }

    SUB_UseTargets();
    SliderStop();
};

void() SliderMove = {
    if (self.height < self.io_received) {
        self.state = SLIDER_STATE_FORWARD;
    } else {
        self.state = SLIDER_STATE_REVERSE;
    }

    vector next_dest = self.pos2 * self.io_received + self.pos1;

    if (next_dest != self.origin) {
        SUB_CalcMove(next_dest, self.speed, SliderStopAtLanding);
        sound(self, CHAN_VOICE, self.noise2, 1, ATTN_NORM);
    }
};

void() CalcScalarPos = {
    float sign = (self.origin - self.pos1) * self.pos2 < 0 ? -1 : 1;
    self.height = vlen(self.origin - self.pos1) / self.t_length * sign;
};

void() SliderRecalcAndMove = {
    CalcScalarPos();
    SliderMove();
};

void() SliderUse = {
    // Cache activator for later use
    self.enemy = activator;

    CalcScalarPos();

    // Fix bug related to triggering with 0 movement
    if (self.height == self.io_received) {
        SliderStop();
        return;
    }

    float behind = self.height < self.io_received;
    float ahead = self.height > self.io_received;
    float nextmove = self.cnt + self.wait;

    if (self.state == SLIDER_STATE_STOPPED) {
        // Start moving eventually
        if (nextmove <= self.ltime) {
            SliderMove();
        } else {
            self.think = SliderRecalcAndMove;
            self.nextthink = nextmove;
        }
    } else if ((self.state == SLIDER_STATE_FORWARD && behind)
            || (self.state == SLIDER_STATE_REVERSE && ahead)) {
        // Keep moving
        // Call again to update destination
        SliderMove();
    } else {
        if (self.wait <= 0) {
            SliderRecalcAndMove();
        } else {
            // Stop & reverse direction
            SliderStop();
            self.think = SliderRecalcAndMove;
            self.nextthink = self.cnt + self.wait;
        }
    }
};

void() SliderInit = {
    entity dest_ent = find(world, targetname, self.target);
    if (!dest_ent) {
        //error("func_slider requires a target!");
        dprint("func_slider ");
        if (self.targetname != "") {
            dprint3("\"", self.targetname, "\" ");
        }
        dprint("requires a target\n");
        return;
    }
    
    // displacement from start to target dest
    self.pos2 = dest_ent.origin - self.pos1;

    // distance from position start to target dest
    self.t_length = vlen(self.pos2);

    self.use = SliderUse;
};

void() SliderObserverThink = {
    // if slider is still blocked
    if (self.owner.ltime == self.pausetime) {
        self.nextthink = time + SLIDER_BLOCKCHECK_DTIME;
        self.think = SliderObserverThink;
    } else {
        self.owner.count = 0;
        remove(self);
    }
};

void() SliderBlocked = {
    if (self.style == SLIDER_DMGSTYLE_CRUSH) {
        T_Damage(other, self, self, self.dmg);
    } else if (self.style == SLIDER_DMGSTYLE_CONST) {
        if (time > self.pausetime) {
            T_Damage(other, self, self, self.dmg);
            other.velocity = '0 0 0';
            
            // next time to inflict damage
            self.pausetime = time + SLIDER_DMG_DTIME;
        }
    } else {
        if (self.count == 0) {
            entity observer = spawn();
            observer.nextthink = time + SLIDER_BLOCKCHECK_DTIME;
            observer.think = SliderObserverThink;

            // slider's ltime when first blocked
            observer.pausetime = self.ltime;
            observer.owner = self;
        }

        if (self.count == 0 || time > self.pausetime) {
            // damage rises quadratically
            float damage = self.dmg * (0.5 * (self.count * self.count) + 1);
            T_Damage(other, self, self, damage);
            other.velocity = '0 0 0';

            // next time to inflict damage
            self.pausetime = time + SLIDER_DMG_DTIME;

            self.count+= 1;
        }
    }
};

void() MoverInitSounds = {
    string stop_snd;
    string move_snd;
    
    switch(self.sounds) {
        case 1: // DOOR Stone
            stop_snd = "doors/drclos4.wav";
            move_snd = "doors/doormv1.wav";
            break;

        case 2: // DOOR Machine
            stop_snd = "doors/hydro2.wav";
            move_snd = "doors/hydro1.wav";
            break;

        case 3: // DOOR Chain
            stop_snd = "doors/stndr2.wav";
            move_snd = "doors/stndr1.wav";
            break;

        case 4: // DOOR Metal
            stop_snd = "doors/ddoor2.wav";
            move_snd = "doors/ddoor1.wav";
            break;

        case 5: // DOOR SEC Medieval
            stop_snd = "doors/latch2.wav";
            move_snd = "doors/winch2.wav";
            break;

        case 6: // DOOR SEC Metal
            stop_snd = "doors/airdoor2.wav";
            move_snd = "doors/airdoor1.wav";
            break;

        case 7: // DOOR SEC Base
            stop_snd = "doors/basesec2.wav";
            move_snd = "doors/basesec1.wav";
            break;

        case 8: // PLAT Base
            stop_snd = "plats/plat2.wav";
            move_snd = "plats/plat1.wav";
            break;

        case 9: // PLAT Chain
            stop_snd = "plats/medplat2.wav";
            move_snd = "plats/medplat1.wav";
            break;

        case 10: // TRAIN Ratchet
            stop_snd = "plats/train2.wav";
            move_snd = "plats/train1.wav";
            break;

        default: // Silent
            stop_snd = "misc/null.wav";
            move_snd = "misc/null.wav";
    }

    if ((!self.noise1) || self.noise1 == "") {
        self.noise1 = stop_snd;
    }

    if ((!self.noise2) || self.noise2 == "") {
        self.noise2 = move_snd;
    }

    precache_sound(self.noise1);
    precache_sound(self.noise2);
};

void() func_slider = {
    self.movetype = MOVETYPE_PUSH;
    self.state = SLIDER_STATE_STOPPED;

    // time since stopping
    self.cnt = -self.wait;

    // current position as scalar
    self.height = 0;

    if (self.speed <= 0) {
        self.speed = 40;
    }

    if (self.dmg <= 0) {
        self.dmg = 2;
    }

    if (self.spawnflags & SLIDER_SPAWNFLAG_SOLID) {
        self.solid = SOLID_BSP;
    } else {
        self.solid = SOLID_NOT;
    }

    // starting point
    self.pos1 = self.origin;

    // number of frames entity is blocked
    self.count = 0;

    setorigin(self, self.origin);
    setmodel(self, self.model);

    self.blocked = SliderBlocked;

    self.think = SliderInit;
    self.nextthink = 0.01;

    MoverInitSounds();
};
