package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import javax.inject.Inject;
import org.hamcrest.CoreMatchers;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;

@RunWith(Arquillian.class)
public class CustomRulesEvaluatorTest extends TransactionalTests {

    @Inject
    private CustomRulesEvaluator customRulesEvaluator;
    
    @Inject
    private RulesService rulesService;
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementAndVerifyReportCreated() {
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        MovementDetails movementDetails = getMovementDetails();
        customRulesEvaluator.evaluate(movementDetails);

        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size() + 1));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerFlagStateRule() throws Exception {
        String flagState = "SWE";
        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setFlagState(flagState);
        
        CustomRule customRule = getCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("ASSET");
        segment.setSubCriteria("FLAG_STATE");
        segment.setCondition("EQ");
        segment.setValue(flagState);
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    
    private MovementDetails getMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setLatitude(11d);
        movementDetails.setLongitude(56d);
        movementDetails.setPositionTime(new Date());
        movementDetails.setSource("INMARSAT_C");
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setFlagState("SWE");
        return movementDetails;
    }
    
    private CustomRule getCustomRule() {
        CustomRule customRule = new CustomRule();
        customRule.setName("Test rule " + RulesTestHelper.getRandomIntegers(7));
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setDescription("Description");
        customRule.setActive(true);
        customRule.setArchived(false);
        customRule.setUpdated(new Date());
        customRule.setUpdatedBy("Test");
        return customRule;
    }
}
