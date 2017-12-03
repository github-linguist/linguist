WarpPreset {
	*new {|path|
		if(path.notNil) {
			^Object.readArchive(path);
		};

		^super.new.init();
	}

	init {

	}

	save {
		Dialog.savePanel({|path|
			this.writeArchive(path);
		});
	}
}