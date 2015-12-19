package com.example.spacegame.model;

import arx.core.vec.ReadVec2f;
import arx.core.vec.ReadVec3f;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/10/15
 * Time: 8:19 PM
 */
public class Ship {
    protected ReadVec2f dimension = new ReadVec2f(1.0f, 1.0f);

    protected ReadVec2f position = new ReadVec2f(0.0f, 0.0f);

    protected ReadVec2f velocity = new ReadVec2f(0.0f, 0.0f);

    protected float acceleration = 0.1f;

    protected float rotation = 0.0f;

    protected float angularAcceleration = 20.0f;

    public ReadVec2f getPosition() {
        return position;
    }

    public void setPosition(ReadVec2f position) {
        this.position = position;
    }

    public ReadVec2f getDimension() {
        return dimension;
    }

    public void setDimension(ReadVec2f dimension) {
        this.dimension = dimension;
    }

    public ReadVec2f getVelocity() {
        return velocity;
    }

    public void setVelocity(ReadVec2f velocity) {
        this.velocity = velocity;
    }

    public float getAcceleration() {
        return acceleration;
    }

    public void setAcceleration(float acceleration) {
        this.acceleration = acceleration;
    }

    public float getRotation() {
        return rotation;
    }

    public void setRotation(float rotation) {
        this.rotation = rotation;
    }

    public float getAngularAcceleration() {
        return angularAcceleration;
    }

    public void setAngularAcceleration(float angularAcceleration) {
        this.angularAcceleration = angularAcceleration;
    }
}
