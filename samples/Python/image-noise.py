import time
import random
import Tkinter
import Image, ImageTk # PIL libray

class App(object):
    def __init__(self, size, root):
        self.root = root
        self.root.title("Image Noise Test")

        self.img = Image.new("RGB", size)
        self.label = Tkinter.Label(root)
        self.label.pack()

        self.time = 0.0
        self.frames = 0
        self.size = size
        self.loop()

    def loop(self):
        self.ta = time.time()
        # 13 FPS boost. half integer idea from C#.
        rnd = random.random
        white = (255, 255, 255)
        black = (0, 0, 0)
        npixels = self.size[0] * self.size[1]
        data = [white if rnd() > 0.5 else black for i in xrange(npixels)]
        self.img.putdata(data)
        self.pimg = ImageTk.PhotoImage(self.img)
        self.label["image"] = self.pimg
        self.tb = time.time()

        self.time += (self.tb - self.ta)
        self.frames += 1

        if self.frames == 30:
            try:
                self.fps = self.frames / self.time
            except:
                self.fps = "INSTANT"
            print ("%d frames in %3.2f seconds (%s FPS)" %
                  (self.frames, self.time, self.fps))
            self.time = 0
            self.frames = 0

        self.root.after(1, self.loop)

def main():
    root = Tkinter.Tk()
    app = App((320, 240), root)
    root.mainloop()

main()
