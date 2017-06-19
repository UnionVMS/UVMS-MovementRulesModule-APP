package eu.europa.ec.fisheries.uvms.rules.service.business.fact;

import eu.europa.ec.fisheries.schema.rules.template.v1.FactType;
import eu.europa.ec.fisheries.schema.sales.IdType;
import eu.europa.ec.fisheries.uvms.rules.service.business.AbstractFact;

public class SalesVesselCountryFact extends AbstractFact {

    private IdType id;

    @Override
    public void setFactType() {
        this.factType = FactType.SALES_VESSEL_COUNTRY;
    }

    public IdType getID() {
        return this.id;
    }

    public void setID(IdType id) {
        this.id = id;
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof SalesVesselCountryFact)) return false;
        final SalesVesselCountryFact other = (SalesVesselCountryFact) o;
        if (!other.canEqual((Object) this)) return false;
        final Object this$id = this.getID();
        final Object other$id = other.getID();
        if (this$id == null ? other$id != null : !this$id.equals(other$id)) return false;
        return true;
    }

    public int hashCode() {
        final int PRIME = 59;
        int result = 1;
        final Object $id = this.getID();
        result = result * PRIME + ($id == null ? 43 : $id.hashCode());
        return result;
    }

    protected boolean canEqual(Object other) {
        return other instanceof SalesVesselCountryFact;
    }
}
