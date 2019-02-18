package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollRequestType;
import eu.europa.ec.fisheries.uvms.mobileterminal.dto.CreatePollResultDto;

import javax.ejb.Stateless;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("asset/internal/poll")
@Stateless
@Consumes(value = { MediaType.APPLICATION_JSON })
@Produces(value = { MediaType.APPLICATION_JSON })
public class AssetModuleMock {

    @POST
    @Path("/")
    public Response createPoll(PollRequestType createPoll) {
        CreatePollResultDto createPollResultDto = new CreatePollResultDto();
        createPollResultDto.setUnsentPoll(false);
        System.setProperty("AssetPollEndpointReached", "True");
        return Response.ok(createPollResultDto).build();
    }

}
