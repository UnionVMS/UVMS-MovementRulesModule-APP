package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;

@Path("/previousReports")
@Stateless
@Consumes(value = { MediaType.APPLICATION_JSON })
@Produces(value = { MediaType.APPLICATION_JSON })
public class PreviousReportRestResource {

    private static final Logger LOG = LoggerFactory.getLogger(PreviousReportRestResource.class);

    @Inject
    private RulesDao rulesDao;

    @GET
    @Path("/list")
    @RequiresFeature(UnionVMSFeature.viewAlarmRules)
    public Response getAllPreviousReports(){
        try {
            List<PreviousReport> previousReports = rulesDao.getPreviousReportList();
            return Response.ok(previousReports).build();
        }catch (Exception e){
            LOG.error("Error while getting a list of all previous reports.", e.getMessage(), e);
            return Response.status(500).entity(ExceptionUtils.getRootCause(e)).build();
        }
    }

    @DELETE
    @Path("/byAsset/{assetGuid}")
    @RequiresFeature(UnionVMSFeature.manageGlobalAlarmsRules)
    public Response deletePreviousReportByAssetGuid(@PathParam("assetGuid") String assetGuid){
        try {
            PreviousReport previousReport = rulesDao.getPreviousReportByAssetGuid(assetGuid);
            rulesDao.deletePreviousReport(previousReport);
            return Response.ok().build();
        }catch (Exception e){
            LOG.error("Error while deleting a previous report", e.getMessage(), e);
            return Response.status(500).entity(ExceptionUtils.getRootCause(e)).build();
        }
    }
}
