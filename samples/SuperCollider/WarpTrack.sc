WarpTrack {
	classvar <defaults;

	var <parent;
	var <settings;

	*initClass {
		defaults['303_1'] = defaults['303'].copy;
		defaults['303_1']['paramControls'].putPairs([
			'Bus 1', 46,
			'Bus 2', 45,
			'Bus 3', 48,
			'echovol', 47
		]);
		defaults['303_1']['params'].putPairs([
			'Bus 1', 0,
			'Bus 2', 0,
			'Bus 3', 0,
			'echovol', 0
		]);

		defaults['303_2'] = defaults['303_1'].copy;
		defaults['303_2']['paramControls'].putPairs([
		]);
		defaults['303_2']['params'].putPairs([
		]);

		defaults['808_1'] = defaults['808'].copy;
		defaults['808_1']['paramControls'].putPairs([
			'bitcrusher', 88,
			'Send 1', 89,
			'Send 2', 90,
			'Send 3', 101
		]);
		defaults['808_1']['params'].putPairs([
			'bitcrusher', 0,
			'Send 1', 0,
			'Send 2', 0,
			'Send 3', 0
		]);

		defaults['808_2'] = defaults['808_1'].copy;
		defaults['808_2']['paramControls'].putPairs([]);
		defaults['808_2']['params'].putPairs([]);
	}

	*new {|argParent, argKey, argMidiChannel, argType|
		^super.new.init(argParent, argKey, argMidiChannel, argType);
	}

	*read {|argParent, path|
		^super.new.init(argParent).readPreset(path);
	}

	*load {|argParent, preset, checkAvailable|
		^super.new.init(argParent).loadPreset(preset, checkAvailable);
	}

	init {|argParent, argKey, argMidiChannel, argType|
		parent = argParent;

		settings = IdentityDictionary[
			'key'			-> argKey,
			'midiChannel'	-> argMidiChannel,
			'notes'			-> Set[],
			'params'		-> IdentityDictionary[],
			'paramControls'	-> IdentityDictionary[],
			'patternTrack'	-> true,
			'sensorFuncs'	-> IdentityDictionary[]
		];

		if(argType.notNil) {
			settings['type'] = argType;
			if(WarpTrack.defaults.keys.includes(argType)) {
				['paramControls', 'params'].do {|key, i|
					settings[key] = WarpTrack.defaults[argType][key];
				}
			};

			this.initParams();
		};
	}

	on {|note|
		var clock = parent.clock;
		var sub = 1 / (parent.clock.tempo * 16); // one sub division

		clock.schedAbs(clock.nextBar - sub, {
			if(settings['patternTrack']) {
				this.allOff();
			} {
				this.off(note);
			};
		});

		parent.clock.playNextBar({
			settings['notes'].add(note);
			parent.noteOn(settings['midiChannel'], note, 127);
		});
	}

	off {|note|
		// settings['notes'].remove(note);
		parent.noteOff(settings['midiChannel'], note, 0);
	}

	hit {|note=60, vel=127, dur=1, quant=0|
		{
			parent.noteOn(settings['midiChannel'], note, vel);
			dur.wait;
			parent.noteOff(settings['midiChannel'], note, vel);
		}.fork(parent.clock, quant:quant);
	}

	allOff {
		settings['notes'].do {|note, i|
			this.off(note);
		};
	}

	assign {|paramKey, num, learn=false, init=true, checkAvailable=true|
		if(num.notNil) {
			this.assignAll(IdentityDictionary[paramKey -> num], learn, init, checkAvailable);
		} {
			parent.assign(settings['key'], paramKey, nil, learn);
		};
	}

	assignAll {|paramControls, learn=false, init=true, checkAvailable=true|
		var action = {
			var channel = settings['midiChannel'];

			paramControls.keysValuesDo { |paramKey, num|
				if(checkAvailable.not ||
					(checkAvailable && parent.isControlAvailable(channel, num))) {
					settings['paramControls'][paramKey] = num;
					if(init) {
						settings['params'][paramKey] = 0;
					};
					parent.setControl(channel, num, paramKey);
					if(learn) {
						parent.control(channel, num, 127);
						0.05.wait;
						parent.control(channel, num, 0);
					};
					paramKey ++ " assigned to controlNum " ++ num;
				} {
					("this controlNum " ++ num ++ " is already assigned!").postln;
				};
			};
		};

		if(learn) {
			action.fork;
		} {
			action.();
		};

	}

	initParams {
		settings['params'].keysValuesDo { |key, value|
			this.setParam(key, value);
		};
		settings['paramControls'].keysValuesDo { |key, value|
			parent.setControl(settings['midiChannel'], value, key);
		};
	}

	setParam {|paramKey, val, quant|
		var func = {
			parent.control(
				settings['midiChannel'],
				settings['paramControls'][paramKey],
				val
			);

			settings['params'][paramKey] = val;
		};

		if(quant.notNil) {
			{
				func.();
			}.fork(parent.clock, quant:quant);
		} {
			func.();
		};
	}

	readPreset {|path, checkAvailable=true|
		this.loadPreset(Object.readArchive(path), checkAvailable);
	}

	loadPreset {|preset, checkAvailable=true|
		// copy all settings except notes and paramControls
		preset.keys.reject({|settingKey, i|
			['notes', 'paramControls'].includes(settingKey);
		}).do {|presetKey, i|
			settings[presetKey] = preset[presetKey];
		};

		// copy notes if it's a patternTrack
		if(preset['patternTrack']) {
			settings['notes'] = preset['notes'];
		};

		// assign all without learn or init
		this.assignAll(
			preset['paramControls'],
			false,
			false,
			checkAvailable
		);

		this.initParams();

		// if(settings['notes'].size > 0) {
		// 	this.on(settings['notes'].asArray[0]);
		// };
	}

	sensor {|sensorKey, val|
		settings['sensorFuncs'][sensorKey].do {|func, i|
			func.(this, val);
		}
	}

	addFunc {|sensorKey, funcKey, func|
		if(parent.sensorKeys.includes(sensorKey)) {
			if(settings['sensorFuncs'].includesKey(sensorKey).not) {
				settings['sensorFuncs'][sensorKey] = IdentityDictionary[];
			};

			settings['sensorFuncs'][sensorKey][funcKey] = func;
		} {
			"parent doesn't have that sensor key".postln;
		};
	}

	removeFunc {|sensorKey, funcKey|
		settings['sensorFuncs'][sensorKey].removeAt(funcKey);

		if(settings['sensorFuncs'][sensorKey].isEmpty) {
			settings['sensorFuncs'].removeAt(sensorKey);
		};
	}

	availableControls {
		^parent.availableControls[settings['midiChannel']].copy;
	}

	save {
		Dialog.savePanel({|path|
			settings.writeArchive(path);
		});
	}

	play {
		if(settings['patternTrack'] && settings['notes'].notEmpty) {
			this.on(settings['notes'].choose);
		};
	}
}
