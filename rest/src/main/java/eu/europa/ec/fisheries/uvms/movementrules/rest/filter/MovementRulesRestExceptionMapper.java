package eu.europa.ec.fisheries.uvms.movementrules.rest.filter;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import java.nio.file.AccessDeniedException;

@Provider
public class MovementRulesRestExceptionMapper implements ExceptionMapper<Exception> {


    private static final Logger LOG = LoggerFactory.getLogger(MovementRulesRestExceptionMapper.class);
    public MovementRulesRestExceptionMapper() {
        super();
    }

    @Override
    public Response toResponse(Exception exception) {
        if(exception instanceof AccessDeniedException){
            AppError error = new AppError(Response.Status.FORBIDDEN.getStatusCode(), ExceptionUtils.getRootCauseMessage(exception));
            return Response.ok(error).header("MDC", MDC.get("requestId")).build();
        }else {
            AppError error = new AppError(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), ExceptionUtils.getRootCauseMessage(exception));
            return Response.ok(error).header("MDC", MDC.get("requestId")).build();
        }

    }
}
