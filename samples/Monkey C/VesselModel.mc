using Toybox.Time;

class StartTimer {

    var startTime = null; // if null: clock is STOPPED, if non-null: clock is RUNNING
    var countDown = new Time.Duration(5 * 60);
    var clock;

    function initialize(clock_) {
    	clock = clock_;
    }
    
    function start() {
    	if (startTime == null) { // cannot re-start timer that's already started
    		startTime = clock.now().add(countDown);
    	}
    }
    
    // Synchronizes running pre-start timer to the closest full minute
    function sync() {
    	var now = clock.now();
    	if (startTime != null && now.compare(startTime) < 0) {
    		var delta = startTime.subtract(now).value();
    		var mins = Math.round((delta + 0.0) / 60.0);
    		startTime = now.add(new Time.Duration(60 * mins));	
    	}
    }
    
    // Fast-forwards timer by 1 minute. E.g., a -4mins pre-start timer goes to -3mins,
    // or a 2mins post-start timer goes to 3mins. 
    function forward() {
    	if (startTime) {
    		// Clock is STARTED
    		startTime = startTime.subtract(new Time.Duration(60));
    	} else {
    		// Clock is STOPPED
    		countDown = countDown.subtract(new Time.Duration(60));
    	}
    }
    
    // Rewinds timer by 1 minute. E.g., a -4mins pre-start timer goes to -5mins,
    // or a 2mins post-start timer goes to 1mins. 
    function backward() {
    	if (startTime) {
    		// Clock is STARTED
    		startTime = startTime.add(new Time.Duration(60));
    	} else {
    		// Clock is STOPPED
    		countDown = countDown.add(new Time.Duration(60));
    	}
    }
    
    function getDuration() {
    	if (startTime) {
    		// Clock is STARTED
    		
    		// Note that Moment.subtract() always yields a positive duration, so
    		// we need to make an explicit decision regarding the sign.
    		if (clock.now().compare(startTime) < 0) {
    			return new Time.Duration(-clock.now().subtract(startTime).value());
    		} else {
    			return clock.now().subtract(startTime);
    		}
    	} else {
    		// Clock is STOPPED
    		return new Time.Duration(-countDown.value());
    	}
    }
}

class FakeClock {
	var epochSeconds = 0;
	
	function now() {
		return new Time.Moment(epochSeconds);
	}
}

class SystemClock {
	function now() {
		return Time.now();
	}
}

class StartTimerTests {
	(:test)
	function newTimer_hasDefaultDuration(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		return timer.getDuration().value() == -300;
	}
	
	(:test)
	function startedTimer_countsDown(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		var success = true;
		
		clock.epochSeconds = 10;
		success &= (timer.getDuration().value() == -290);
		
		clock.epochSeconds = 290;
		success &= (timer.getDuration().value() == -10);
		
		clock.epochSeconds = 300;
		success &= (timer.getDuration().value() == 0);
		
		clock.epochSeconds = 310;
		success &= (timer.getDuration().value() == 10);
		
		return success;
	}
	
	(:test)
	function sync_isNoOpOnUnstartedTimer(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.sync();
		return timer.startTime == null && timer.getDuration().value() == -300;
	}
	
	(:test)
	function sync_isNoOpInPostStart(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		clock.epochSeconds = 310; // 10 seconds post-start
		timer.sync();
		return timer.getDuration().value() == 10;
	}
	
	(:test)
	function sync_syncsToClosestMinute(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		var success = true;
		
		// Initial condition
		success &= timer.startTime.value() == 300 && timer.getDuration().value() == -300;
		
		// No-op when exactly on 4 minutes
		clock.epochSeconds = 60;
		timer.sync();
		success &= timer.startTime.value() == 300 && timer.getDuration().value() == -240;
		
		// Sync from 3:50 to 4:00
		clock.epochSeconds = 70;
		timer.sync();
		success &= timer.startTime.value() == 310 && timer.getDuration().value() == -240;
		
		// Sync from 3:29 to 3:00
		clock.epochSeconds = 101;
		timer.sync();
		success &= timer.startTime.value() == 281 && timer.getDuration().value() == -180;
		
		// Sync from 0:09 to 0:00
		clock.epochSeconds = 272;
		timer.sync();
		success &= timer.startTime.value() == 272 && timer.getDuration().value() == 0;
		
		return success;
	}
	
	(:test)
	function forward_whenNotStarted_decreasesPrestartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		
		timer.forward();
		return timer.startTime == null && timer.getDuration().value() == -240;
	}
	
	(:test)
	function forward_whenStarted_decreasesPrestartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		
		clock.epochSeconds = 60;
		timer.forward();
		return timer.startTime.value() == 240 && timer.getDuration().value() == -180;
	}
	
	(:test)
	function forward_whenStarted_increasesPostStartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		
		clock.epochSeconds = 360;
		timer.forward();
		return timer.startTime.value() == 240 && timer.getDuration().value() == 120;
	}
	
	(:test)
	function backward_whenNotStarted_increasesPrestartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		
		timer.backward();
		return timer.startTime == null && timer.getDuration().value() == -360;
	}
	
	(:test)
	function backward_whenStarted_decreasesPrestartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		
		clock.epochSeconds = 60;
		timer.backward();
		return timer.startTime.value() == 360 && timer.getDuration().value() == -300;
	}
	
	(:test)
	function backward_whenStarted_increasesPostStartTime(logger) {
		var clock = new FakeClock();
		var timer = new StartTimer(clock);
		timer.start();
		
		clock.epochSeconds = 360;
		timer.backward();
		return timer.startTime.value() == 360 && timer.getDuration().value() == 0;
	}
}
