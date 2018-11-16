package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.CustomRulesEvaluator;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;

@Path("/internal")
@Stateless
public class InternalRestResources {

    @Inject
    RulesServiceBean rulesService;
    
    @Inject
    private CustomRulesEvaluator customRuleEvaluator;
    

    @POST
    @Consumes(value = { MediaType.APPLICATION_JSON })
    @Produces(value = { MediaType.APPLICATION_JSON })
    @Path("/tickets-and-rules-by-movement")
    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovementsEvent(GetTicketsAndRulesByMovementsRequest request) throws Exception {
        return rulesService.getTicketsAndRulesByMovements(request.getMovementGuids());
    }
    
    
    @POST
    @Path("/evaluate")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Response evaluateCustomRules(MovementDetails movementDetails) {
        customRuleEvaluator.evaluate(movementDetails);
        return Response.ok().build();
    }
}
