package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import org.slf4j.MDC;

import javax.ejb.Stateless;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/movement/rest/internal")
@Consumes(value = {MediaType.APPLICATION_JSON})
@Produces(value = {MediaType.APPLICATION_JSON})
@Stateless
public class MovementRestMock {

    @GET
    @Path("/countMovementsInDateAndTheDayBeforeForAsset/{id}")
    public Response countMovementsInDateAndTheDayBeforeForAsset(@PathParam("id") String id, @QueryParam("after") String after) {   //yyyy-MM-dd HH:mm:ss Z
        return Response.ok().entity(0).type(MediaType.APPLICATION_JSON)
                .header("MDC", MDC.get("requestId")).build();
    }
}
