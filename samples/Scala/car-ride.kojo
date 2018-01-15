// Kojo examples (files ending in .kojo) are licensed under The MIT License:

// Copyright (C) 2009-2018 Lalit Pant <pant.lalit@gmail.com> and the Kojo Dev Team.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Use the four arrow keys to avoid the blue cars
// You gain energy every second, and lose energy for every collision
// You lose if your energy drops below zero, or you hit the edges of the screen
// You win if you stay alive for a minute
switchToDefault2Perspective()
val carHeight = 100
val markerHeight = 80
// The collision polygon for the (very similarly sized) car images car1.png and car2.png
val carE = trans(2, 14) -> Picture {
    repeat(2) {
        forward(70); right(45); forward(20); right(45)
        forward(18); right(45); forward(20); right(45)
    }
}
def car(img: String) = PicShape.image(img, carE)

val cars = collection.mutable.Map.empty[Picture, Vector2D]
val carSpeed = 3
val pResponse = 3
var pVel = Vector2D(0, 0)
var disabledTime = 0L

val bplayer = newMp3Player
val cplayer = newMp3Player

def createCar() {
    val c = trans(cb.x + random(cb.width.toInt), cb.y + cb.height) -> car("/media/car-ride/car2.png")
    draw(c)
    cars += c -> Vector2D(0, -carSpeed)
}
val markers = collection.mutable.Set.empty[Picture]
def createMarker() {
    val mwidth = 20
    val m = fillColor(white) * penColor(white) *
        trans(cb.x + cb.width / 2 - mwidth / 2, cb.y + cb.height) -> PicShape.rect(markerHeight, mwidth)
    draw(m)
    markers += m
}

cleari()
drawStage(darkGray)
val cb = canvasBounds
val player = car("/media/car-ride/car1.png")
draw(player)
drawAndHide(carE)

timer(1200) {
    createMarker()
    createCar()
}

animate {
    player.moveToFront()
    val enabled = epochTimeMillis - disabledTime > 300
    if (enabled) {
        if (isKeyPressed(Kc.VK_LEFT)) {
            pVel = Vector2D(-pResponse, 0)
            player.transv(pVel)
        }
        if (isKeyPressed(Kc.VK_RIGHT)) {
            pVel = Vector2D(pResponse, 0)
            player.transv(pVel)
        }
        if (isKeyPressed(Kc.VK_UP)) {
            pVel = Vector2D(0, pResponse)
            player.transv(pVel)
            if (!isMp3Playing) {
                playMp3Sound("/media/car-ride/car-accel.mp3")
            }
        }
        else {
            stopMp3()
        }
        if (isKeyPressed(Kc.VK_DOWN)) {
            pVel = Vector2D(0, -pResponse)
            player.transv(pVel)
            if (!bplayer.isMp3Playing) {
                bplayer.playMp3Sound("/media/car-ride/car-brake.mp3")
            }
        }
        else {
            bplayer.stopMp3()
        }
    }
    else {
        player.transv(pVel)
    }

    if (player.collidesWith(stageLeft) || player.collidesWith(stageRight)) {
        cplayer.playMp3Sound("/media/car-ride/car-crash.mp3")
        player.setOpacity(0.5)
        drawMessage("You Crashed!", red)
        stopAnimation()
    }
    else if (player.collidesWith(stageTop)) {
        pVel = Vector2D(0, -pResponse)
        player.transv(pVel * 2)
        disabledTime = epochTimeMillis
    }
    else if (player.collidesWith(stageBot)) {
        pVel = Vector2D(0, pResponse)
        player.transv(pVel * 2)
        disabledTime = epochTimeMillis
    }

    cars.foreach { cv =>
        val (c, vel) = cv
        c.moveToFront()
        if (player.collidesWith(c)) {
            cplayer.playMp3Sound("/media/car-ride/car-crash.mp3")
            pVel = bouncePicVectorOffPic(player, pVel - vel, c) / 2
            player.transv(pVel * 3)
            c.transv(-pVel * 3)
            disabledTime = epochTimeMillis
            updateEnergyCrash()
        }
        else {
            val newVel = Vector2D(vel.x + randomDouble(1) / 2 - 0.25, vel.y)
            cars += c -> newVel
            c.transv(newVel)
        }
        if (c.position.y + carHeight < cb.y) {
            c.erase()
            cars -= c
        }
    }
    markers.foreach { m =>
        m.translate(0, -carSpeed * 2)
        if (m.position.y + markerHeight < cb.y) {
            m.erase()
            markers -= m
        }
    }
}

var energyLevel = 0
def energyText = s"Energy: $energyLevel"
val energyLabel = trans(cb.x + 10, cb.y + cb.height - 10) -> PicShape.textu(energyText, 20, blue)
def updateEnergyTick() {
    energyLevel += 2
    energyLabel.update(energyText)
}
def updateEnergyCrash() {
    energyLevel -= 10
    energyLabel.update(energyText)
    if (energyLevel < 0) {
        drawMessage("You're out of energy! You Lose", red)
        stopAnimation()
    }
}

def drawMessage(m: String, c: Color) {
    val te = textExtent(m, 30)
    val pic = penColor(c) * trans(cb.x + (cb.width - te.width) / 2, 0) -> PicShape.text(m, 30)
    draw(pic)
}

def manageGameScore() {
    var gameTime = 0
    val timeLabel = trans(cb.x + 10, cb.y + 50) -> PicShape.textu(gameTime, 20, blue)
    draw(timeLabel)
    draw(energyLabel)
    timeLabel.forwardInputTo(stageArea)

    timer(1000) {
        gameTime += 1
        timeLabel.update(gameTime)
        updateEnergyTick()

        if (gameTime == 60) {
            drawMessage("Time up! You Win", green)
            stopAnimation()
        }
    }
}

manageGameScore()
playMp3Loop("/media/car-ride/car-move.mp3")
activateCanvas()

// Car images, via google images, from http://motor-kid.com/race-cars-top-view.html 
// and www.carinfopic.com
// Car sounds from http://soundbible.com
