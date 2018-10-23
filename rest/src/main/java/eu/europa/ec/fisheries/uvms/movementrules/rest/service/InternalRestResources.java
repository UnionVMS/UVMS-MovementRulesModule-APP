package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.uvms.movementrules.rest.dto.ResponseCode;
import eu.europa.ec.fisheries.uvms.movementrules.rest.dto.ResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesEventServiceBean;

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

    @POST
    @Consumes(value = { MediaType.APPLICATION_JSON })
    @Produces(value = { MediaType.APPLICATION_JSON })
    @Path("/tickets-and-rules-by-movement")
    public ResponseDto<GetTicketsAndRulesByMovementsResponse> getTicketsAndRulesByMovementsEvent(GetTicketsAndRulesByMovementsRequest request) throws Exception {
        return new ResponseDto<>(rulesEventServiceBean.getTicketsAndRulesByMovementsEvent(request), ResponseCode.OK);
    }
}
