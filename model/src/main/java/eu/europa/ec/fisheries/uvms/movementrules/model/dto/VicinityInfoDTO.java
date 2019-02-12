package eu.europa.ec.fisheries.uvms.movementrules.model.dto;


public class VicinityInfoDTO {
    String asset;
    double distance;
    String movementID;

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
