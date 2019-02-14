package eu.europa.ec.fisheries.uvms.movementrules.model.dto;

import java.util.UUID;

public class VicinityInfoDTO {
    String asset;
    double distance;
    String movementID;

    public VicinityInfoDTO() {
    }

    public VicinityInfoDTO(UUID asset, UUID movementID, double distance) {
        this.asset = asset.toString();
        this.distance = distance;
        this.movementID = movementID.toString();
    }

    public String getAsset() {
        return asset;
    }

    public void setAsset(String asset) {
        this.asset = asset;
    }

    public double getDistance() {
        return distance;
    }

    public void setDistance(double distance) {
        this.distance = distance;
    }

    public String getMovementID() {
        return movementID;
    }

    public void setMovementID(String movementID) {
        this.movementID = movementID;
    }
}
