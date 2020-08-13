package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.schema.exchange.module.v1.SetCommandRequest;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.v1.*;

import javax.ejb.Stateless;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Date;
import java.util.UUID;

@Path("/exchange/rest/")
@Stateless
@Consumes(value = {MediaType.APPLICATION_JSON})
@Produces(value = {MediaType.APPLICATION_JSON})
public class ExchangeModuleRestMock {

    @POST
    @Path(value = "unsecured/api/sendEmail")
    public Response sendEmail(EmailType email) {
        System.setProperty("ExchangeEmailEndpointReached", "True");
        return Response.ok().build();
    }

    @POST
    @Path(value = "unsecured/api/pluginCommand")
    public Response sendCommandToPlugin(String commandRequest) {
        System.setProperty("ExchangeEmailEndpointReached", "True");
        return Response.ok().build();
    }


}
