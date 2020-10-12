package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian.tests;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.rest.filter.AppError;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian.BuildRulesRestDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian.RulesTestHelper;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class CustomRulesRestResourceTest extends BuildRulesRestDeployment {

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRuleTest() {
        createCustomRule();
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRuleWithTimeIntervalTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        CustomRuleIntervalType intervalType = new CustomRuleIntervalType();
        intervalType.setStart(DateUtils.dateToEpochMilliseconds(Instant.now()));
        intervalType.setEnd(DateUtils.dateToEpochMilliseconds(Instant.now()
                .plus(10, ChronoUnit.MINUTES)));
        customRule.getTimeIntervals().add(intervalType);

        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRuleWithActionEmailTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        CustomRuleActionType emailAction = new CustomRuleActionType();
        emailAction.setAction(ActionType.EMAIL);
        emailAction.setValue("jarvis@consid.se");
        emailAction.setOrder("0");

        customRule.getActions().add(emailAction);

        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByUserTest() {
        CustomRuleType created = createCustomRule();

        List<CustomRuleType> found = getWebTarget()
                .path("/customrules/listAll")
                .path(created.getUpdatedBy())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(new GenericType<List<CustomRuleType>>() {
                });

        boolean present = found.stream()
                .map(CustomRuleType::getGuid)
                .anyMatch(id -> created.getGuid().equals(id));
        assertTrue(present);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleByGuidTest() {
        CustomRuleType created = createCustomRule();

        CustomRuleType found = getWebTarget()
                .path("/customrules")
                .path(created.getGuid())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(CustomRuleType.class);

        assertNotNull(found);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleByGuid_ShouldFailWithInvalidGuidTest() {
        Response response = getWebTarget()
                .path("/customrules")
                .path(UUID.randomUUID().toString())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get();

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), appError.code.intValue());
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateCustomRuleTest() {
        CustomRuleType created = createCustomRule();
        final String newName = "NEW_TEST_NAME";
        created.setName(newName);

        CustomRuleType updated = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(created), new GenericType<CustomRuleType>() {
                });

        assertEquals(newName, updated.getName());
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateSubscriptionTest() {
        CustomRuleType created = createCustomRule();
        SubscriptionType st = created.getSubscriptions().get(0);

        assertEquals(st.getType(), SubscriptionTypeType.TICKET);

        UpdateSubscriptionType ust = new UpdateSubscriptionType();
        ust.setSubscription(st);
        ust.setRuleGuid(created.getGuid());
        ust.setOperation(SubscritionOperationType.REMOVE);

        CustomRuleType updated = getWebTarget()
                .path("/customrules/subscription")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(ust), CustomRuleType.class);

        assertTrue(updated.getSubscriptions().isEmpty());
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateSubscription_ShouldFailInvalidSubscriptionTest() {
        Response response = getWebTarget()
                .path("/customrules/subscription")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(new UpdateSubscriptionType()));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), appError.code.intValue());
    }

    @Test
    @OperateOnDeployment("normal")
    public void deleteCustomRuleTest() {
        CustomRuleType created = createCustomRule();
        assertTrue(created.isActive());

        CustomRuleType deleted = getWebTarget()
                .path("/customrules/")
                .path(created.getGuid())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .delete(CustomRuleType.class);

        assertFalse(deleted.isActive());
    }


    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidNameTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setName(" ");
        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidDescriptionTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setDescription(" ");
        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidActionEmailTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        CustomRuleActionType emailAction = new CustomRuleActionType();
        emailAction.setAction(ActionType.EMAIL);
        emailAction.setValue("jarvis@consid,se");
        emailAction.setOrder("0");

        customRule.getActions().add(emailAction);

        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidTimeIntervalsTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        CustomRuleIntervalType intervalType = new CustomRuleIntervalType();
        intervalType.setStart(DateUtils.dateToEpochMilliseconds(Instant.now()));
        customRule.getTimeIntervals().add(intervalType);

        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());

        intervalType = new CustomRuleIntervalType();
        intervalType.setEnd(DateUtils.dateToEpochMilliseconds(Instant.now()));
        customRule.getTimeIntervals().add(intervalType);

        response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());

        intervalType = new CustomRuleIntervalType();
        intervalType.setStart(DateUtils.dateToEpochMilliseconds(Instant.now()));
        intervalType.setEnd(DateUtils.dateToEpochMilliseconds(Instant.now()
                .minus(10, ChronoUnit.MINUTES)));
        customRule.getTimeIntervals().add(intervalType);

        response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidSegmentTypeLogicalOperatorTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleSegmentType segmentType = customRule.getDefinitions().get(0);
        segmentType.setLogicBoolOperator(LogicOperatorType.NONE);

        Response response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        segmentType = customRule.getDefinitions().get(1);
        segmentType.setLogicBoolOperator(LogicOperatorType.AND);

        response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        segmentType = customRule.getDefinitions().get(1);
        segmentType.setLogicBoolOperator(LogicOperatorType.OR);

        response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidSegmentTypeStartEndOperatorTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleSegmentType segmentType = customRule.getDefinitions().get(0);
        segmentType.setStartOperator(")");

        Response response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        segmentType = customRule.getDefinitions().get(0);
        segmentType.setEndOperator("(");

        response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.BAD_REQUEST.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void createCustomRule_ShouldFailInvalidAvailabilityTypeTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setAvailability(AvailabilityType.GLOBAL);

        Response response = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Status.FORBIDDEN.getStatusCode(), appError.code.intValue());
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateCustomRule_ShouldFailInvalidCustomRule() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        Response response = getWebTarget().path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(customRule));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), appError.code.intValue());
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByQuery_ShouldFailInvalidQueryTest() {
        createCustomRule();
        CustomRuleQuery customRuleQuery = new CustomRuleQuery();

        Response response = getWebTarget()
                .path("/customrules/listByQuery")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRuleQuery));

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), appError.code.intValue());
    }

    private CustomRuleType createCustomRule() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule), new GenericType<CustomRuleType>() {
                });
        assertNotNull(created.getGuid());
        return created;
    }
}
