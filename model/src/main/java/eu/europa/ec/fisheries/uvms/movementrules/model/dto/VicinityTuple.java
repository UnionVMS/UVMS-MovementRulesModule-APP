package eu.europa.ec.fisheries.uvms.movementrules.model.dto;

import java.util.UUID;

public class VicinityTuple {
    UUID asset;
    double distance;
    UUID movementID;

    public UUID getAsset() {
        return asset;
    }

    public void setAsset(UUID asset) {
        this.asset = asset;
    }

    public double getDistance() {
        return distance;
    }

    public void setDistance(double distance) {
        this.distance = distance;
    }

    public UUID getMovementID() {
        return movementID;
    }

    public void setMovementID(UUID movementID) {
        this.movementID = movementID;
    }
}
