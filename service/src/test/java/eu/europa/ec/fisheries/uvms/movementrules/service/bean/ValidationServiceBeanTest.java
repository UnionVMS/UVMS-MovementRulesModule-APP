package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Calendar;
import java.util.Date;
import javax.inject.Inject;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;

@RunWith(Arquillian.class)
public class ValidationServiceBeanTest extends TransactionalTests {

    @Inject
    ValidationServiceBean validationService;

    @Inject
    RulesServiceBean rulesService;

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredLastTriggeredDateShouldBeSetTest() throws Exception {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MILLISECOND, 0);
        Date timestamp = calendar.getTime();

        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        MovementDetails movementFact = RulesTestHelper.createBasicMovementDetails();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "EMAIL,test@test.com");

        CustomRule updatedCustomRule = rulesService.getCustomRuleByGuid(createdCustomRule.getGuid());
        String lastTriggered = DateUtils.dateToString(updatedCustomRule.getTriggered());
        assertThat(lastTriggered, is(notNullValue()));

        Date dateTriggered = DateUtils.stringToDate(lastTriggered);
        assertTrue(dateTriggered.getTime() >= timestamp.getTime());
    }

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredNewTicketShouldBeCreatedTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long openTicketsBefore = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());

        MovementDetails movementFact = RulesTestHelper.createBasicMovementDetails();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "EMAIL,test@test.com");

        long openTicketsAfter = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        assertThat(openTicketsAfter, is(openTicketsBefore + 1));
    }

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredSendToNAFTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long openTicketsBefore = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());

        MovementDetails movementFact = RulesTestHelper.createBasicMovementDetails();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "SEND_TO_NAF,SWE");

        long openTicketsAfter = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        assertThat(openTicketsAfter, is(openTicketsBefore + 1));
    }
}
