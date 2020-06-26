package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.CustomRulesEvaluator;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import org.hamcrest.CoreMatchers;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class PreviousReportRestResourceTest extends BuildRulesRestDeployment {

    @Inject
    RulesValidator rulesValidator;

    @Before
    public void initClass(){
        rulesValidator.updateCustomRules();
    }

    @Test
    @OperateOnDeployment("normal")
    public void testGetAllPreviousReports(){
        MovementDetails movement = RulesTestHelper.createBasicMovementDetails();

        Response evaluate = getWebTarget()
                .path("internal")
                .path("evaluate")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(movement));
        assertThat(evaluate.getStatus(), CoreMatchers.is(Response.Status.OK.getStatusCode()));

        evaluate = getAllPreviousReportByRest();
        assertEquals(200, evaluate.getStatus());
        String previousReports = evaluate.readEntity(String.class);
        assertTrue(previousReports.contains(movement.getAssetGuid()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void testGetAllPreviousReports_VerifyMobileTerminalIdIsPresent(){
        MovementDetails movement = RulesTestHelper.createBasicMovementDetails();

        Response evaluate = getWebTarget()
                .path("internal")
                .path("evaluate")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(movement));
        assertThat(evaluate.getStatus(), CoreMatchers.is(Response.Status.OK.getStatusCode()));

        evaluate = getAllPreviousReportByRest();
        assertEquals(200, evaluate.getStatus());
        String previousReports = evaluate.readEntity(String.class);
        assertTrue(previousReports.contains(movement.getMovementGuid()));
        assertTrue(previousReports.contains(movement.getMobileTerminalGuid()));

    }

    @Test
    @OperateOnDeployment("normal")
    public void testDeleteAPreviousReport(){
        MovementDetails movement = RulesTestHelper.createBasicMovementDetails();
        Response evaluate = getWebTarget()
                .path("internal")
                .path("evaluate")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(movement));
        assertThat(evaluate.getStatus(), CoreMatchers.is(Response.Status.OK.getStatusCode()));

        Response response = getAllPreviousReportByRest();
        assertEquals(200, response.getStatus());
        String previousReports = response.readEntity(String.class);
        assertTrue(previousReports.contains(movement.getAssetGuid()));

        Response delete = getWebTarget()
                .path("previousReports/byAsset/")
                .path(movement.getAssetGuid())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .delete(Response.class);

        assertEquals(200, delete.getStatus());

        response = getAllPreviousReportByRest();
        assertEquals(200, response.getStatus());
        previousReports = response.readEntity(String.class);
        assertFalse(previousReports.contains(movement.getAssetGuid()));

    }

    @Test
    @OperateOnDeployment("normal")
    public void testDontCreatePreviousReportOnLongTermParked(){
        MovementDetails movement = RulesTestHelper.createBasicMovementDetails();
        movement.setLongTermParked(true);

        Response evaluate = getWebTarget()
                .path("internal")
                .path("evaluate")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(movement));
        assertThat(evaluate.getStatus(), CoreMatchers.is(Response.Status.OK.getStatusCode()));

        evaluate = getAllPreviousReportByRest();
        assertEquals(200, evaluate.getStatus());
        String previousReports = evaluate.readEntity(String.class);
        assertFalse(previousReports.contains(movement.getAssetGuid()));
    }


    private Response getAllPreviousReportByRest(){
        return getWebTarget()
                .path("previousReports/list/")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Response.class);
    }
}
