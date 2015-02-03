WarpUtil {
	var <>parent;
	var <curSensor;

	var <win;
	var <texts;
	var <sensorSlider;
	var <sliders;
	var <updateRout;

	*new {|argParent|
		^super.new.init(argParent);
	}

	init {|argParent|
		parent = argParent;
		texts = IdentityDictionary[];

		this.makeView();
		this.startUpdate();
	}

	calibrate {
		{
			parent.doAdjusts.size.do {|i|
				parent.sensorMaxs[i] = 0;
				parent.sensorMins[i] = 9999;
				parent.doAdjusts[i] = true;
			};

			sliders.do {|slider, i|
				if(parent.sensorKeys[i] !== curSensor) {
					slider.valueAction_(0);
					0.1.wait;
					slider.valueAction_(1);
				};
			}
		}.fork(AppClock);
	}

	stopCalibrate {
		parent.doAdjusts.size.do {|i|
			parent.doAdjusts[i] = false;
		}
	}

	makeView {
		var width = 370;

		if(win.notNil) {
			win.close;
		};

		win = Window("Sensor Inspector", Rect(256, 10, width, 424)).front;
		win.view.addFlowLayout;

		['sensorVals', 'sensorPrevs', 'sensorMins', 'sensorMaxs'].do {|label, i|
			StaticText(win, (width * 0.5)@20)
				.string_(label.asString);

			win.view.decorator.nextLine;


			texts[label] = StaticText(win, (width / 2)@40);

			if(curSensor.notNil) {
				texts[label].string_(
					"\t" ++ parent.perform(label).at(
						parent.sensorKeys.indexOf(curSensor)
					);
				);
			};

			win.view.decorator.nextLine;
		};

		sensorSlider = EZSlider(win, 280@20, label: curSensor.asString);

		sliders = parent.sensorKeys.collect { |sensorKey, i|
			EZSlider(win, 280@20, label: sensorKey.asString)
				.action_({|ez|
					NetAddr.localAddr.sendMsg(
						"/prox/" ++ sensorKey,
						ez.value * 100
					);
				});
		};
	}

	startUpdate {
		updateRout = Routine {
			inf.do {|i|
				var index;

				if(curSensor.notNil) {
					index = parent.sensorKeys.indexOf(curSensor);

					['sensorVals', 'sensorPrevs', 'sensorMins',
						'sensorMaxs'].do {|label, i|

							texts[label]
							.string_("\t" ++ parent.perform(label)[index]);
					};

					sensorSlider.value = parent.sensorVals[index].linlin(
						parent.sensorMins[index],
						parent.sensorMaxs[index],
						0,
						1
					);
				};
				0.05.wait;
			}
		};

		updateRout.play(AppClock);
	}

	stopUpdate {
		updateRout.stop();
	}

	curSensor_ {|argCurSensor|
		curSensor = argCurSensor;
		this.makeView();
	}
}