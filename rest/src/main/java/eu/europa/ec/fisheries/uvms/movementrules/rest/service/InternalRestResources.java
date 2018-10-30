package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.CustomRulesEvaluator;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesEventServiceBean;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/internal")
@Stateless
public class InternalRestResources {

    @Inject
    RulesEventServiceBean rulesEventServiceBean;
    
    @Inject
    private CustomRulesEvaluator customRuleEvaluator;
    

    @POST
    @Consumes(value = { MediaType.APPLICATION_JSON })
    @Produces(value = { MediaType.APPLICATION_JSON })
    @Path("/tickets-and-rules-by-movement")
    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovementsEvent(GetTicketsAndRulesByMovementsRequest request) throws Exception {
        return rulesEventServiceBean.getTicketsAndRulesByMovementsEvent(request);
    }
    
    
    @POST
    @Path("/evaluate")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @RequiresFeature(UnionVMSFeature.manageAlarmRules)
    public void evaluateCustomRules(MovementDetails movementDetails) {
        customRuleEvaluator.evaluate(movementDetails);
    }
}
