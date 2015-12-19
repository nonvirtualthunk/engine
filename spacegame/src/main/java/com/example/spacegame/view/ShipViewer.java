package com.example.spacegame.view;

import arx.core.vec.ReadVec4f;
import arx.engine.simple.Canvas;
import arx.graphics.Image;
import arx.resource.ResourceManager;
import com.example.spacegame.model.Ship;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/10/15
 * Time: 8:27 PM
 */
public class ShipViewer {


    public void drawShip(Ship ship, Canvas canvas) {
        Image image = ResourceManager.getImage("images/spaceship.png", true);
        canvas.drawQuad(ship.getPosition(), ship.getDimension(), ship.getRotation(), new ReadVec4f(1,1,1,1), image);
    }
}
