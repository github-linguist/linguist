
/*
 * This file is part of Jkop
 * Copyright (c) 2016 Job and Esther Technologies, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

public class SEButtonEntity : SESpriteEntity, SEPointerListener
{
	class SEImageButtonEntity : SEButtonEntity
	{
		property SEImage image_normal;
		property SEImage image_hover;
		property SEImage image_pressed;

		public void update() {
			if(get_pressed()) {
				var img = image_pressed;
				if(img == null) {
					img = image_hover;
				}
				if(img == null) {
					img = image_normal;
				}
				set_image(img);
			}
			else if(get_has_pointer()) {
				var img = image_hover;
				if(img == null) {
					img = image_normal;
				}
				set_image(img);
			}
			else {
				set_image(image_normal);
			}
		}
	}

	class SETextButtonEntity : SEButtonEntity
	{
		property String button_text;
		property String normal_font;
		property String hover_font;
		property String pressed_font;

		public void update() {
			if(get_pressed()) {
				var ff = pressed_font;
				if(String.is_empty(ff)) {
					ff = hover_font;
				}
				if(String.is_empty(ff)) {
					ff = normal_font;
				}
				set_text(button_text, ff);
			}
			else if(get_has_pointer()) {
				var ff = hover_font;
				if(String.is_empty(ff)) {
					ff = normal_font;
				}
				set_text(button_text, ff);
			}
			else {
				set_text(button_text, normal_font);
			}
		}
	}

	public static SEButtonEntity for_image(SEImage img) {
		return(SEButtonEntity.for_images(img, null, null));
	}

	public static SEButtonEntity for_images(SEImage normal, SEImage hover, SEImage pressed) {
		return(new SEImageButtonEntity().set_image_normal(normal).set_image_hover(hover)
			.set_image_pressed(pressed));
	}

	public static SEButtonEntity for_text(String text, String normal_font = null, String hover_font = null, String pressed_font = null) {
		return(new SETextButtonEntity().set_button_text(text).set_normal_font(normal_font).set_hover_font(hover_font)
			.set_pressed_font(pressed_font));
	}

	property SEMessageListener listener;
	property Object data;
	bool pressed = false;
	bool has_pointer = false;

	public bool get_pressed() {
		return(pressed);
	}

	public bool get_has_pointer() {
		return(has_pointer);
	}

	public void initialize(SEResourceCache rsc) {
		base.initialize(rsc);
		update();
	}

	public virtual void update() {
	}

	public virtual void on_pointer_enter(SEPointerInfo pi) {
		if(has_pointer) {
			return;
		}
		has_pointer = true;
		update();
	}

	public virtual void on_pointer_leave(SEPointerInfo pi) {
		if(has_pointer == false && pressed == false) {
			return;
		}
		has_pointer = false;
		pressed = false;
		update();
	}

	public void on_pointer_move(SEPointerInfo pi) {
		if(pi.is_inside(get_x(), get_y(), get_width(), get_height())) {
			if(has_pointer == false) {
				on_pointer_enter(pi);
			}
		}
		else {
			if(has_pointer) {
				on_pointer_leave(pi);
			}
		}
	}

	public void on_pointer_press(SEPointerInfo pi) {
		if(pressed) {
			return;
		}
		if(pi.is_inside(get_x(), get_y(), get_width(), get_height()) == false) {
			return;
		}
		pressed = true;
		update();
	}

	public void on_pointer_release(SEPointerInfo pi) {
		if(pressed == false) {
			return;
		}
		if(pi.is_inside(get_x(), get_y(), get_width(), get_height()) == false) {
			return;
		}
		on_pointer_click(pi);
		pressed = false;
		update();
	}

	public virtual void on_pointer_click(SEPointerInfo pi) {
		if(listener != null) {
			listener.on_message(data);
		}
	}
}
