import string

var lcd_display = module('lcd_display')

#  ------------------------------------------------------------------------------------
#  This is a Tasmota driver for the Liquid Crystal LCD displays that use the I2C bus.
#  The backlight is on by default, since that is the most likely operating mode in
#  most cases. It is a direct berry port of the Arduino Liquid Crystal 12C C++ library.
#  Cols and Rows both start at 1, not zero.
#  -------------------------------------------------------------------------------------

class lcd_i2c

    # commands
    static LCD_CLEARDISPLAY = 0x01
    static LCD_RETURNHOME = 0x02
    static LCD_ENTRYMODESET = 0x04
    static LCD_DISPLAYCONTROL = 0x08
    static LCD_CURSORSHIFT = 0x10
    static LCD_FUNCTIONSET = 0x20
    static LCD_SETCGRAMADDR = 0x40
    static LCD_SETDDRAMADDR = 0x80

    # flags for display entry mode
    static LCD_ENTRYRIGHT = 0x00
    static LCD_ENTRYLEFT = 0x02
    static LCD_ENTRYSHIFTINCREMENT = 0x01
    static LCD_ENTRYSHIFTDECREMENT = 0x00

    # flags for display on/off control
    static LCD_DISPLAYON = 0x04
    static LCD_DISPLAYOFF = 0x00
    static LCD_CURSORON = 0x02
    static LCD_CURSOROFF = 0x00
    static LCD_BLINKON = 0x01
    static LCD_BLINKOFF = 0x00

    # flags for display/cursor shift
    static LCD_DISPLAYMOVE = 0x08
    static LCD_CURSORMOVE = 0x00
    static LCD_MOVERIGHT = 0x04
    static LCD_MOVELEFT = 0x00

    # flags for function set
    static LCD_8BITMODE = 0x10
    static LCD_4BITMODE = 0x00
    static LCD_2LINE = 0x08
    static LCD_1LINE = 0x00
    static LCD_5x10DOTS = 0x04
    static LCD_5x8DOTS = 0x00

    # flags for backlight control
    static LCD_BACKLIGHT = 0x08
    static LCD_NOBACKLIGHT = 0x00

    # Helper flags
    static LCD_LINES = {1: 0x80, 2: 0xC0, 3: 0x94, 4: 0xD4}
    static LCD_SLEEP = 1

    static Rg = 0x00    # Register default
    static En = 0x04    # Enable bit
    static Rs = 0x01    # Register select bit

    var address, wire, rows, cols, charsize, backlight, displaycontrol, displaymode

    #  Constructor
    #
    #  @param address    I2C slave address of the LCD display. Most likely printed on the
    #                    LCD circuit board, or look in the supplied LCD documentation.
    #  @param cols       Number of columns your LCD display has.
    #  @param rows       Number of rows your LCD display has.
    #  @param charsize   The size in dots that the display has, use LCD_5x10DOTS or LCD_5x8DOTS.
    def init(address, rows, cols, charsize)
        self.address = address ? address : 0x27
        self.rows = rows ? rows : 4
        self.cols = cols ? cols : 20
        self.charsize = charsize ? charsize : self.LCD_5x8DOTS
        self.backlight = self.LCD_BACKLIGHT
        self.wire = tasmota.wire_scan(self.address)
        # Start the display
        self.begin()
    end
    def begin()
        # Set some defaults...
        var displayfunction = self.LCD_4BITMODE | self.LCD_1LINE | self.LCD_5x8DOTS
        if self.rows > 1
            displayfunction |= self.LCD_2LINE
        end
        # For some 1 line displays you can select a 10 pixel high font
        if self.charsize != 0 && self.rows == 1
            displayfunction |= self.LCD_5x10DOTS
        end
        # Reeset expander and turn backlight off (Bit 8 =1)
        self.expand_write(self.backlight)
        tasmota.delay(1)
        # We start in 8bit mode, try to set 4 bit mode
	    self.write4bits(0x03 << 4)
        # Wait min 4 ms
        tasmota.delay(5)
        # second try
	    self.write4bits(0x03 << 4)
	    tasmota.delay(5) # wait min 4.1ms
    	# third go!
	    self.write4bits(0x03 << 4)
    	tasmota.delay(1)
        # set to 4-bit interface
        self.write4bits(0x02 << 4)
        # Set lines, font size, etc.
	    self.command(self.LCD_FUNCTIONSET | displayfunction)
        # Turn the display on with no cursor or blinking default
	    self.displaycontrol = self.LCD_DISPLAYON | self.LCD_CURSOROFF | self.LCD_BLINKOFF
    	self.display()
        # Clear the screen
        self.clear()
        # Initialize to default text direction (for roman languages)
	    self.displaymode = self.LCD_ENTRYLEFT | self.LCD_ENTRYSHIFTDECREMENT
        # Set the entry mode
        self.command(self.LCD_ENTRYMODESET | self.displaymode)
        # Set cursor position to zero
        self.home()
    end

    #********** High level commands, for the user! **********

    def clear()
        self.command(self.LCD_CLEARDISPLAY)
    end
    # Set cursor position to zero
    def home()
        self.command(self.LCD_RETURNHOME)
        tasmota.delay(2)
    end
    # Set cursor position. First row and column is 1 not zero 
    def set_cursor(col, row)
        var row_offsets = [0x00, 0x40, 0x14, 0x54]
        if row > self.rows
            row = self.rows-1 
        end
        self.command(self.LCD_SETDDRAMADDR | (col-1 + row_offsets[row-1]))
    end
    # Turn the display on/off (quickly)
    def no_display()
        self.displaycontrol &= ~self.LCD_DISPLAYON
        self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    def display()
        self.displaycontrol |= self.LCD_DISPLAYON
        self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    # Turns the underline cursor on/off
    def no_cursor()
	    self.displaycontrol &= ~self.LCD_CURSORON
	self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    def cursor()
	    self.displaycontrol |= self.LCD_CURSORON
	self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    # Turn on and off the blinking cursor
    def no_blink()
	    self.displaycontrol &= ~self.LCD_BLINKON
	self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    def blink()
	    self.displaycontrol |= self.LCD_BLINKOFF
	    self.command(self.LCD_DISPLAYCONTROL | self.displaycontrol)
    end
    # This is for text that flows Left to Right
    def display_left()
        self.command(self.LCD_CURSORSHIFT | self.LCD_DISPLAYMOVE | self.LCD_MOVELEFT)
    end
    def display_right()
        self.command(self.LCD_CURSORSHIFT | self.LCD_DISPLAYMOVE | self.LCD_MOVERIGHT)
    end
    # This is for text that flows Left to Right
    def left_to_right()
        self.displaymode |= self.LCD_ENTRYLEFT
        self.command(self.LCD_ENTRYMODESET | self.displaymode)
    end
    # This is for text that flows Right to Left
    def right_to_left()
        self.displaymode &= ~self.LCD_ENTRYLEFT
        self.command(self.LCD_ENTRYMODESET | self.displaymode)
    end
    # This will 'right justify' text from the cursor
    def autoscroll()
        self.displaymode |= self.LCD_ENTRYSHIFTINCREMENT
        self.command(self.LCD_ENTRYMODESET | self.displaymode)
    end    
    # This will 'left justify' text from the cursor
    def no_autoscroll()
        self.displaymode &= ~self.LCD_ENTRYSHIFTINCREMENT
        self.command(self.LCD_ENTRYMODESET | self.displaymode)
    end
    # Allows us to fill the first 8 CGRAM locations with custom characters 
    def create_char(location, chars)
        location &= 0x7 # We only have 8 locations 0-7
        self.command(self.LCD_SETCGRAMADDR | (location << 3))
        for char: 0 .. 7
            self.write(chars[char])
        end
    end
    # Turn the (optional) backlight on/off
    def backlight_on()
        self.backlight = self.LCD_BACKLIGHT
        self.expand_write(0)
    end
    def backlight_off()
        self.backlight = self.LCD_NOBACKLIGHT
        self.expand_write(0)
    end
    def get_backlight()
        return self.backlight == self.LCD_BACKLIGHT
    end

    # *********** Mid level commands, for sending data/cmds ************

    def command(byte)
        self.send(byte, 0)
    end
    def write(byte)
        self.send(byte, self.Rs)
    end
    def write_line(text, line)
        if !self.LCD_LINES.has(line) return end
        import string
        self.command(self.LCD_LINES[line])
        text = self.pad(text)
        for char: 0 .. size(text) -1
            self.write(string.byte(text[char]))
        end
    end
    
    # ************ low level data pushing commands ************

    def send(byte, mode)
        mode = mode ? mode : 0
        self.write4bits(mode | (byte & 0xF0) )
        self.write4bits(mode | ((byte << 4) & 0xF0) )
    end
    def write4bits(byte)
        self.expand_write(byte)
        self.pulse_enable(byte)
    end
    def expand_write(byte)
        if self.wire != nil
            self.wire.write(self.address, self.Rg, (byte | self.backlight), 1)
        end
    end
    def pulse_enable(byte)
        self.expand_write(byte | self.En)
        tasmota.delay(self.LCD_SLEEP)
        self.expand_write(byte & ~self.En)
        tasmota.delay(self.LCD_SLEEP)
    end

    # Helper methods

    def pad(text)
        for x: size(text) .. self.cols-1  
            text += " " 
        end
        return text        
    end
    def set_backlight(on)
        if on
            self.backlight_on()
        else
            self.backlight_off()
        end
    end
end

class clock
    static callback = nil
    static initialised = false
    static waiting = false
    static def initialise()
        clock.initialised = true
        if clock.waiting
            clock.waiting = false
            clock.callback()
            clock.set_timer()
        end
    end
    static def start(callback)
        clock.callback = callback
        if clock.initialised
            clock.set_timer()
        else
            clock.waiting = true
        end
    end
    static def set_timer()
        tasmota.set_timer(
            def()
                var l = tasmota.rtc()['local']
                var t = tasmota.time_dump(l)
                return 60000-t['sec']*1000
            end(),
            def() 
                if clock.callback
                    clock.callback()
                end
                clock.set_timer() 
            end, 
            "hc_lcd_timer"
        )
    end
    static def stop()
        tasmota.remove_timer("hc_lcd_timer")
    end
 end

class dotmatrixscreen
    var lcd
    def init()
        self.lcd = lcd_i2c()
    end
    def start()
        self.power(true)
        # subscribe to initial time
        if clock.initialised 
            self.update_clock()
        else
            self.lcd.write_line("Starting...", 1)
            clock.callback = / -> self.update_clock()
        end
        self.start_clock()
        tasmota.add_rule("HeatingDisplay#HeatingZone", /z-> self.update_zone(z))
        tasmota.add_rule("HeatingDisplay#ClearZone", /z-> self.clear_zone(z))
    end
    def stop()
        self.lcd.clear()
        tasmota.remove_rule("HeatingDisplay#HeatingZone")
        tasmota.remove_rule("HeatingDisplay#ClearZone")
        self.stop_clock()
        self.power(false)
    end
    def power(bool)
        self.lcd.set_backlight(bool)
    end
    def update_clock()
        self.lcd.write_line(tasmota.strftime('%H:%M %a %d %b %y', tasmota.rtc()['local']), 1)
    end
    def update_zone(zone)
        if zone['id'] > 3 return end
        var const = 12&(1<<zone['mode'])
        var fmt = const ? "%s %s Const" : '%s %s until %%R'
        var info = string.format(fmt, zone['label'], zone['power'] ? 'ON' : 'OFF')
        if !const
            info = tasmota.strftime(info, zone['expiry'])
        end
        self.lcd.write_line(info, zone['id']+1)
    end
    def clear_zone(zone)
        self.lcd.write_line('', zone+1)
    end
    def start_clock()
        clock.start(/->self.update_clock())
    end
    def stop_clock()
        clock.stop()
    end
end

# Hold a reference to screen instance
lcd_display._display = nil

def ack()
    tasmota.publish_result('{"HeatingDisplay":"ACK"}', 'RESULT')
end

def start()
    if lcd_display._display return end
    lcd_display._display = dotmatrixscreen()
    lcd_display._display.start()
end
def stop()
    if !lcd_display._display return end
    lcd_display._display.stop()
    lcd_display._display = nil
end

# Subscibe to Tasmota events
tasmota.add_rule("Time#Initialized", /-> clock.initialise())
# Subscribe to events sent from Heating Controller
tasmota.add_rule("HeatingDisplay==ON", /->tasmota.set_timer(0, start))
tasmota.add_rule("HeatingDisplay==OFF", /->tasmota.set_timer(0, stop))
tasmota.add_rule("HeatingDisplay==SYN", /->tasmota.set_timer(0, ack))
# Once lvgl_display loads an initialisation trigger is broadcast
ack()

return lcd_display
