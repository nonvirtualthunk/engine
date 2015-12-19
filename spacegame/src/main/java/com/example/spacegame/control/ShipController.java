package com.example.spacegame.control;

import arx.core.vec.ReadVec2f;
import com.example.spacegame.model.Ship;
import org.lwjgl.glfw.GLFW;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/13/15
 * Time: 12:26 PM
 */
public class ShipController {
    protected Map<Integer,Boolean> keyMap = new HashMap<Integer, Boolean>();

    public void updateShip (Ship myShip, float deltaSeconds) {
        updateVelocity(myShip, deltaSeconds);

        updatePosition(myShip, deltaSeconds);

        updateRotation(myShip, deltaSeconds);
    }

    protected void updateVelocity(Ship myShip, float deltaSeconds) {
        float moveX = 0.0f;
        float moveY = 0.0f;

        if (isKeyDown(GLFW.GLFW_KEY_W)) {
            moveY = deltaSeconds;
        } else if (isKeyDown(GLFW.GLFW_KEY_S)) {
            moveY = -deltaSeconds;
        }

        if (isKeyDown(GLFW.GLFW_KEY_A)) {
            moveX = -deltaSeconds;
        } else if (isKeyDown(GLFW.GLFW_KEY_D)) {
            moveX = deltaSeconds;
        }

        if (isKeyDown(GLFW.GLFW_KEY_SPACE)) {
            moveX = -myShip.getPosition().x();
            moveY = -myShip.getPosition().y();
        }

        ReadVec2f oldVel = myShip.getVelocity();
        ReadVec2f newVel = new ReadVec2f(oldVel.x() + moveX, oldVel.y() + moveY);
        myShip.setVelocity(newVel);
    }

    protected void updatePosition(Ship myShip, float deltaSeconds) {
        ReadVec2f newVel = myShip.getVelocity();
        ReadVec2f oldPos = myShip.getPosition();
        ReadVec2f newPos = new ReadVec2f(oldPos.x() + newVel.x() * myShip.getAcceleration(),
                oldPos.y() + newVel.y() * myShip.getAcceleration());
        myShip.setPosition(newPos);
    }

    protected void updateRotation(Ship myShip, float deltaSeconds) {
        float rotateBy = 0.0f;

        if (isKeyDown(GLFW.GLFW_KEY_LEFT)) {
            rotateBy = deltaSeconds;
        } else if (isKeyDown(GLFW.GLFW_KEY_RIGHT)) {
            rotateBy = -deltaSeconds;
        }

        float oldRot = myShip.getRotation();
        float newRot = oldRot + rotateBy * myShip.getAngularAcceleration();
        myShip.setRotation(newRot);
    }

    protected boolean isKeyDown (int key) {
        return keyMap.containsKey(key) && keyMap.get(key);
    }


    public Map<Integer, Boolean> getKeyMap() {
        return keyMap;
    }

    public void setKeyMap(Map<Integer, Boolean> keyMap) {
        this.keyMap = keyMap;
    }
}
