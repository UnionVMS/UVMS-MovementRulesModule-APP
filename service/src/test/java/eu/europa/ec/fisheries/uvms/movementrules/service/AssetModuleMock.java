package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollRequestType;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.dto.SimpleCreatePoll;

import javax.ejb.Stateless;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("asset/rest/internal/")
@Stateless
@Consumes(value = { MediaType.APPLICATION_JSON })
@Produces(value = { MediaType.APPLICATION_JSON })
public class AssetModuleMock {

    @POST
    @Path("/poll")
    public Response createPoll(PollRequestType createPoll) {
        System.setProperty("AssetPollEndpointReached", "True");
        return Response.ok().entity(Boolean.TRUE).build();
    }

    @POST
    @Path("/createPollForAsset/{id}")
    public Response createPoll(@PathParam("id") String assetId, @QueryParam("username") String username, SimpleCreatePoll createPoll) {
        System.setProperty("AssetPollEndpointReached", "True");
        return Response.ok().entity(Boolean.TRUE).build();
    }

}
