package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Asset;

import java.io.Serializable;
import java.util.Objects;

public class AssetMTEnrichmentResponse implements Serializable {

    public AssetMTEnrichmentResponse(){

    }

    private MobileTerminalType mobileTerminalType;
    private Asset asset;

    public MobileTerminalType getMobileTerminalType() {
        return mobileTerminalType;
    }

    public void setMobileTerminalType(MobileTerminalType mobileTerminalType) {
        this.mobileTerminalType = mobileTerminalType;
    }

    public Asset getAsset() {
        return asset;
    }

    public void setAsset(Asset asset) {
        this.asset = asset;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AssetMTEnrichmentResponse that = (AssetMTEnrichmentResponse) o;
        return Objects.equals(mobileTerminalType, that.mobileTerminalType) &&
                Objects.equals(asset, that.asset);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mobileTerminalType, asset);
    }
}
