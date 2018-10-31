/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */
package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import java.io.InputStream;
import java.util.Date;
import java.util.List;
import javax.annotation.PostConstruct;
import javax.ejb.ConcurrencyManagement;
import javax.ejb.ConcurrencyManagementType;
import javax.ejb.Lock;
import javax.ejb.LockType;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.inject.Inject;
import org.drools.template.parser.DefaultTemplateContainer;
import org.drools.template.parser.TemplateContainer;
import org.drools.template.parser.TemplateDataListener;
import org.kie.api.KieServices;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.service.ValidationService;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.SanityRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleParser;

@Startup
@Singleton
@ConcurrencyManagement(ConcurrencyManagementType.CONTAINER)
public class RulesValidator {
    private static final Logger LOG = LoggerFactory.getLogger(RulesValidator.class);
    private static final String CUSTOM_RULE_DRL_FILE = "src/main/resources/rules/CustomRules.drl";
    private static final String CUSTOM_RULE_TEMPLATE = "/templates/CustomRulesTemplate.drt";

    private static final String SANITY_RULES_DRL_FILE = "src/main/resources/rules/SanityRules.drl";
    private static final String SANITY_RULES_TEMPLATE = "/templates/SanityRulesTemplate.drt";

    @Inject
    private ValidationService validationService;

    private KieContainer sanityKcontainer;
    private List<SanityRule> currentSanityRules;

    private KieContainer customKcontainer;

    @PostConstruct
    public void init() {
        updateSanityRules();
        updateCustomRules();
    }
    
    @Lock(LockType.WRITE)
    public void updateSanityRules() {
        try {
            // Fetch sanity rules from DB
            List<SanityRule> sanityRules = validationService.getSanityRules();
            if (sanityRules != null && !sanityRules.isEmpty()) {
                if (checkForChanges(sanityRules)) {
                    currentSanityRules = sanityRules;
                    // Add sanity rules
                    String drl = generateSanityRuleDrl(SANITY_RULES_TEMPLATE, sanityRules);

                    KieServices kieServices = KieServices.Factory.get();

                    KieFileSystem sanityKfs = KieServices.Factory.get().newKieFileSystem();

                    sanityKfs.write(SANITY_RULES_DRL_FILE, drl);
                    kieServices.newKieBuilder(sanityKfs).buildAll();
                    sanityKcontainer = kieServices.newKieContainer(kieServices.getRepository().getDefaultReleaseId());
                }
            } else {
                sanityKcontainer = null;
            }
        } catch (RulesServiceException | MovementRulesFaultException  e) {
            LOG.error("Error when getting sanity rules", e);
        }
    }

    @Lock(LockType.WRITE)
    public void updateCustomRules() {
        LOG.info("Updating custom rules");
        try {
            // Fetch custom rules from DB
            List<CustomRule> customRules = validationService.getRunnableCustomRules();
            if (customRules != null && !customRules.isEmpty()) {
                // Add custom rules
                List<CustomRuleDto> rules = CustomRuleParser.parseRules(customRules);
                String drl = generateCustomRuleDrl(CUSTOM_RULE_TEMPLATE, rules);

                KieServices kieServices = KieServices.Factory.get();

                KieFileSystem customKfs = kieServices.newKieFileSystem();

                customKfs.write(CUSTOM_RULE_DRL_FILE, drl);

                // Create session
                kieServices.newKieBuilder(customKfs).buildAll();
                customKcontainer = kieServices.newKieContainer(kieServices.getRepository().getDefaultReleaseId());
            } else {
                customKcontainer = null;
            }
        } catch (RulesServiceException | MovementRulesFaultException  e) {
            LOG.error("Error when getting custom rules", e);
        }
    }

    @Lock(LockType.READ)
    public void evaluate(RawMovementFact fact) {
        if (sanityKcontainer != null) {
            LOG.info("Verify sanity rules");

            KieSession ksession = sanityKcontainer.newKieSession();

            // Inject beans
            ksession.setGlobal("validationService", validationService);
            ksession.setGlobal("logger", LOG);

            ksession.insert(fact);
            ksession.fireAllRules();
        }
    }

    public void evaluateSanityWODrools(RawMovementFact fact) throws RulesServiceException {
        if(fact.getPositionTime() == null){  //Time missing
            LOG.info("\t==> Executing RULE 'Sanity rule 1 - Time missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Time missing", fact);
        }

        if(fact.getLatitude() == null){  //Lat missing
            LOG.info("\t==> Executing RULE 'Sanity rule 2 - Lat missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Lat missing", fact);
        }

        if(fact.getLongitude() == null){  //Long missing
            LOG.info("\t==> Executing RULE 'Sanity rule 3 - Long missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Long missing", fact);
        }

        if((fact.getMobileTerminalConnectId() == null || fact.getMobileTerminalConnectId().isEmpty()) && fact.getPluginType().equals("SATELLITE_RECEIVER")){  //Transponder not found
            LOG.info("\t==> Executing RULE 'Sanity rule 4 - Transponder not found'");
            fact.setOk(false);
            validationService.createAlarmReport("Transponder not found", fact);
        }

        if(fact.getAssetGuid() == null || fact.getAssetGuid().isEmpty()){  //Asset not found
            LOG.info("\t==> Executing RULE 'Sanity rule 5 - Asset not found'");
            fact.setOk(false);
            validationService.createAlarmReport("Asset not found", fact);
        }

        if((fact.getMobileTerminalMemberNumber() == null || fact.getMobileTerminalMemberNumber().isEmpty()) && fact.getPluginType().equals("SATELLITE_RECEIVER") && fact.getMobileTerminalType().equals("INMARSAT_C")){  //Mem No. missing
            LOG.info("\t==> Executing RULE 'Sanity rule 6 - Mem No. missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Mem No. missing", fact);
        }

        if((fact.getMobileTerminalDnid() == null || fact.getMobileTerminalDnid().isEmpty()) && fact.getPluginType().equals("SATELLITE_RECEIVER") && fact.getMobileTerminalType().equals("INMARSAT_C")){  //DNID missing
            LOG.info("\t==> Executing RULE 'Sanity rule 7 - DNID missing'");
            fact.setOk(false);
            validationService.createAlarmReport("DNID missing", fact);
        }

        if((fact.getMobileTerminalSerialNumber() == null || fact.getMobileTerminalSerialNumber().isEmpty()) && fact.getPluginType().equals("SATELLITE_RECEIVER") && fact.getMobileTerminalType().equals("IRIDIUM")){  //Serial No. missing
            LOG.info("\t==> Executing RULE 'Sanity rule 8 - Serial No. missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Serial No. missing", fact);
        }

        if(fact.getComChannelType() == null || fact.getComChannelType().isEmpty()){  //ComChannel Type missing
            LOG.info("\t==> Executing RULE 'Sanity rule 9 - ComChannel Type missing'");
            fact.setOk(false);
            validationService.createAlarmReport("ComChannel Type missing", fact);
        }

        if(((fact.getCfr() == null || fact.getCfr().isEmpty()) && (fact.getIrcs() == null || fact.getIrcs().isEmpty())) && (fact.getPluginType().equals("FLUX") || fact.getComChannelType().equals("MANUAL"))){  //CFR and IRCS missing
            LOG.info("\t==> Executing RULE 'Sanity rule 10 - CFR and IRCS missing'");
            fact.setOk(false);
            validationService.createAlarmReport("CFR and IRCS missing", fact);
        }

        if(fact.getPluginType() == null || fact.getPluginType().isEmpty()){  //Plugin Type missing
            LOG.info("\t==> Executing RULE 'Sanity rule 11 - Plugin Type missing'");
            fact.setOk(false);
            validationService.createAlarmReport("Plugin Type missing", fact);
        }

        if(fact.getPositionTime() != null && fact.getPositionTime().after(new Date())){  //Time in the future
            LOG.info("\t==> Executing RULE 'Sanity rule 12 - Time in the future'" + "[" + fact.getPositionTime() + " > " + new Date() + "]");
            fact.setOk(false);
            validationService.createAlarmReport("Time in future", fact);
        }

        if(fact.getStatusCode() == null || fact.getStatusCode().isEmpty()){  //statusCode must exist
            LOG.info("\t==> Executing RULE 'Sanity check - statusCode must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - statusCode must exist", fact);
        }

        if(fact.getReportedSpeed() == null){  //reportedSpeed must exist
            LOG.info("\t==> Executing RULE 'Sanity check - reportedSpeed must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - reportedSpeed must exist", fact);
        }

        if(fact.getReportedCourse() == null){  //reportedCourse must exist
            LOG.info("\t==> Executing RULE 'Sanity check - reportedCourse must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - reportedCourse must exist", fact);
        }

        if(fact.getMovementType() == null || fact.getMovementType().isEmpty()){  //movementType must
            LOG.info("\t==> Executing RULE 'Sanity check - movementType must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - movementType must exist", fact);
        }

        if(fact.getSource() == null || fact.getSource().isEmpty()){  //source must exist
            LOG.info("\t==> Executing RULE 'Sanity check - source must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - source must exist", fact);
        }

        if(fact.getActivityCallback() == null || fact.getActivityCallback().isEmpty()){  //activityCallback must exist
            LOG.info("\t==> Executing RULE 'Sanity check - activityCallback must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - activityCallback must exist", fact);
        }

        if(fact.getActivityMessageId() == null || fact.getActivityMessageId().isEmpty()){  //activityMessageId must exist
            LOG.info("\t==> Executing RULE 'Sanity check - activityMessageId must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - activityMessageId must exist", fact);
        }

        if(fact.getActivityMessageType() == null || fact.getActivityMessageType().isEmpty()){  //activityMessageType must exist
            LOG.info("\t==> Executing RULE 'Sanity check - activityMessageType must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - activityMessageType must exist", fact);
        }


        /*   no such thing as asset type in rawMovementFact
        //  fact : RawMovementFact((assetType == null) && (pluginType == "FLUX" || comChannelType == "MANUAL"))
        if(fact.ass() == null){  //assetType must exist
            LOG.info("\t==> Executing RULE 'Sanity check - assetType must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - assetType must exist", fact);
        }*/

        if(fact.getAltitude() == null){  //Altitude must exist
            LOG.info("\t==> Executing RULE 'Sanity check - altitude must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - altitude must exist", fact);
        }

        //so, we check to se if we have a movement guid b4 we have sent anything to movement.......   Only works bc the mapper sets a random guid........
        if(fact.getMovementGuid() == null || fact.getMovementGuid().isEmpty()){  //movementGuid must exist
            LOG.info("\t==> Executing RULE 'Sanity check - movementGuid must exist'");
            fact.setOk(false);
            validationService.createAlarmReport("Sanity check - movementGuid must exist", fact);
        }
    }

    @Lock(LockType.READ)
    public void evaluate(MovementFact fact) {
        if (customKcontainer != null) {
            LOG.info("Verify user defined rules");

            KieSession ksession = customKcontainer.newKieSession();

            // Inject beans
            ksession.setGlobal("validationService", validationService);
            ksession.setGlobal("logger", LOG);

            ksession.insert(fact);
            ksession.fireAllRules();
        }
    }
    
    @Lock(LockType.READ)
    public void evaluate(MovementDetails fact) {
        if (customKcontainer != null) {
            LOG.info("Verify user defined rules");

            KieSession ksession = customKcontainer.newKieSession();

            // Inject beans
            ksession.setGlobal("validationService", validationService);
            ksession.setGlobal("logger", LOG);

            ksession.insert(fact);
            ksession.fireAllRules();
        }
    }

    private String generateCustomRuleDrl(String template, List<CustomRuleDto> ruleDtos) {
        InputStream templateStream = this.getClass().getResourceAsStream(template);
        TemplateContainer tc = new DefaultTemplateContainer(templateStream);
        TemplateDataListener listener = new TemplateDataListener(tc);

        int rowNum = 0;
        for (CustomRuleDto ruleDto : ruleDtos) {
            listener.newRow(rowNum, 0);
            listener.newCell(rowNum, 0, ruleDto.getRuleName(), 0);
            listener.newCell(rowNum, 1, ruleDto.getExpression(), 0);
            listener.newCell(rowNum, 2, ruleDto.getAction(), 0);
            listener.newCell(rowNum, 3, ruleDto.getRuleGuid(), 0);
            rowNum++;
        }
        listener.finishSheet();
        String drl = listener.renderDRL();

        LOG.debug("Custom rule file:\n{}", drl);

        return drl;
    }

    private String generateSanityRuleDrl(String template, List<SanityRule> sanityRules) {
        InputStream templateStream = this.getClass().getResourceAsStream(template);
        TemplateContainer tc = new DefaultTemplateContainer(templateStream);
        TemplateDataListener listener = new TemplateDataListener(tc);

        int rowNum = 0;
        for (SanityRule sanityRule : sanityRules) {
            listener.newRow(rowNum, 0);
            listener.newCell(rowNum, 0, sanityRule.getName(), 0);
            listener.newCell(rowNum, 1, sanityRule.getExpression(), 0);
            rowNum++;
        }
        listener.finishSheet();
        String drl = listener.renderDRL();

        LOG.debug("Sanity rule file:\n{}", drl);

        return drl;
    }

    private boolean checkForChanges(List<SanityRule> sanityRules) {
        if (currentSanityRules == null || sanityRules.size() != currentSanityRules.size()) {
            return true;
        } else {
            for (int i = 0; i < sanityRules.size(); i++) {
                SanityRule a = sanityRules.get(i);
                SanityRule b = currentSanityRules.get(i);

                if (a.getDescription() != null && !a.getDescription().equalsIgnoreCase(b.getDescription())) {
                    return true;
                }
                if (a.getDescription() == null && b.getDescription() != null) {
                    return true;
                }
                if (a.getExpression() != null && !a.getExpression().equalsIgnoreCase(b.getExpression())) {
                    return true;
                }
                if (a.getExpression() == null && b.getExpression() != null) {
                    return true;
                }
                if (a.getName() != null && !a.getName().equalsIgnoreCase(b.getName())) {
                    return true;
                }
                if (a.getName() == null && b.getName() != null) {
                    return true;
                }
                if (a.getUpdated() != null && !a.getUpdated().equals(b.getUpdated())) {
                    return true;
                }
                if (a.getUpdated() == null && b.getUpdated() != null) {
                    return true;
                }
                if (a.getUpdatedBy() != null && !a.getUpdatedBy().equalsIgnoreCase(b.getUpdatedBy())) {
                    return true;
                }
                if (a.getUpdatedBy() == null && b.getUpdatedBy() != null) {
                    return true;
                }
            }
            return false;
        }
    }

}