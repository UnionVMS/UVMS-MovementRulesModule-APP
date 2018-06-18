package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.is;
import java.util.Date;
import java.util.List;
import javax.ejb.EJBTransactionRolledbackException;
import javax.inject.Inject;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdList;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdType;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdList;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdType;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementComChannelType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementPoint;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;

@RunWith(Arquillian.class)
public class MovementReportProcessorBeanTest extends TransactionalTests {

    @Inject
    MovementReportProcessorBean movementReport;

    @Inject
    RulesService rulesService;
    
    @Test(expected = EJBTransactionRolledbackException.class)
    public void setMovementReportReceivedNullInputTest() throws Exception {
        movementReport.setMovementReportReceived(null, null, null);
    }
    
    @Test
    public void setMovementReportReceivedPreviousReportShouldBeCreatedTest() throws Exception {
        RawMovementType rawMovement = getBasicRawMovementType();
        
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        movementReport.setMovementReportReceived(rawMovement, "NAF", "TEST");
        
        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size() + 1));
    }
    
    @Test
    public void setMovementReportReceivedPreviousReportShouldBeCreatedWithoutMTTest() throws Exception {
        RawMovementType rawMovement = getBasicRawMovementType();
        
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        rawMovement.setMobileTerminal(null);
        rawMovement.setPluginType("NAF");
        movementReport.setMovementReportReceived(rawMovement, "NAF", "TEST");
        
        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size() + 1));
    }
    
    @Test
    public void setMovementReportReceivedTriggerSanityRuleTest() throws Exception {
        RawMovementType rawMovement = getBasicRawMovementType();
        
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        rawMovement.getPosition().setLatitude(null);
        movementReport.setMovementReportReceived(rawMovement, "NAF", "TEST");
        
        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size()));
    }
    
    @Test
    public void setMovementReportReceivedPreviousReportShouldBeCreatedWithoutAssetIdListTest() throws Exception {
        RawMovementType rawMovement = getBasicRawMovementType();
        
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        rawMovement.setAssetId(null);
        movementReport.setMovementReportReceived(rawMovement, "NAF", "TEST");
        
        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size() + 1));
    }

    private RawMovementType getBasicRawMovementType() {
        RawMovementType movement = new RawMovementType();
        AssetId assetId = new AssetId();
        assetId.setAssetType(AssetType.VESSEL);
        AssetIdList assetIdList = new AssetIdList();
        assetIdList.setIdType(AssetIdType.IRCS);
        assetIdList.setValue("IRCS1234");
        assetId.getAssetIdList().add(assetIdList);
        movement.setAssetId(assetId);
        movement.setAssetName("Test Asset");
        movement.setFlagState("SWE");
        movement.setDateRecieved(new Date());
        movement.setMovementType(MovementTypeType.POS);
        movement.setPluginName("PLUGIN");
        movement.setPluginType("SATELLITE_RECEIVER");
        MobileTerminalType mobileTerminal = new MobileTerminalType();
        IdList idList1 = new IdList();
        idList1.setType(IdType.DNID);
        idList1.setValue("TEST_DNID");
        mobileTerminal.getMobileTerminalIdList().add(idList1);
        IdList idList2 = new IdList();
        idList2.setType(IdType.MEMBER_NUMBER);
        idList2.setValue("TEST_MEMBER_NUMBER");
        mobileTerminal.getMobileTerminalIdList().add(idList2);
        movement.setMobileTerminal(mobileTerminal);
        MovementPoint movementPoint = new MovementPoint();
        movementPoint.setLatitude(56d);
        movementPoint.setLongitude(11d);
        movement.setPosition(movementPoint);
        movement.setPositionTime(new Date());
        movement.setReportedCourse(50d);
        movement.setReportedSpeed(5d);
        movement.setSource(MovementSourceType.INMARSAT_C);
        movement.setComChannelType(MovementComChannelType.NAF);
        return movement;
    }
}
