package com.example.spacegame;

import arx.core.vec.ReadVec2f;
import arx.engine.SimpleEngine;
import arx.engine.simple.Canvas;
import com.example.spacegame.control.ShipController;
import com.example.spacegame.model.Ship;
import com.example.spacegame.view.ShipViewer;
import org.lwjgl.glfw.GLFW;

import java.util.HashMap;
import java.util.Map;

/**
 *
 */
public class SpaceGameMain extends SimpleEngine {
    protected Ship myShip = new Ship();

    protected ShipViewer myViewer = new ShipViewer();

    protected ShipController myController = new ShipController();

    @Override
    public void draw(Canvas canvas) {
        myViewer.drawShip(myShip, canvas);
    }

    @Override
    public void update(float deltaSeconds) {
        myController.updateShip(myShip,deltaSeconds);
    }

    @Override
    public void keyPressed(int key, int mods) {
        myController.getKeyMap().put(key, true);
    }

    @Override
    public void keyReleased(int key, int mods) {
        myController.getKeyMap().put(key, false);
    }

    public static void main(String[] args) {
        SpaceGameMain game = new SpaceGameMain();
        game.run();
    }
}
