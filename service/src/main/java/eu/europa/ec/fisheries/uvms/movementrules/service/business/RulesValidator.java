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
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleParser;

@Startup
@Singleton
@ConcurrencyManagement(ConcurrencyManagementType.CONTAINER)
public class RulesValidator {
    private static final Logger LOG = LoggerFactory.getLogger(RulesValidator.class);
    private static final String CUSTOM_RULE_DRL_FILE = "src/main/resources/rules/CustomRules.drl";
    private static final String CUSTOM_RULE_TEMPLATE = "/templates/CustomRulesTemplate.drt";
    private static final String CUSTOM_VICINITY_RULE_TEMPLATE = "/templates/CustomRuleVicinityListTemplate.drt";

    String valuePattern = "\".*?\"";
    String dividerPattern = "(&&)|(\\|\\|)";


    @Inject
    private ValidationServiceBean validationService;

    @Inject
    private RulesServiceBean rulesService;

    private KieContainer customKcontainer;

    @PostConstruct
    public void init() {
        updateCustomRules();
    }
    

    @Lock(LockType.WRITE)
    public void updateCustomRules() {
        LOG.info("Updating custom rules");
            // Fetch custom rules from DB
            List<CustomRule> customRules = rulesService.getRunnableCustomRules();
            if (customRules != null && !customRules.isEmpty()) {
                // Add custom rules
                List<CustomRuleDto> rules = CustomRuleParser.parseRules(customRules);
                String drl = generateCustomRuleDrl(CUSTOM_RULE_TEMPLATE, rules);

                KieServices kieServices = KieServices.Factory.get();

                KieFileSystem customKfs = kieServices.newKieFileSystem();

                customKfs.write(CUSTOM_RULE_DRL_FILE, drl);

                LOG.error(drl);
                LOG.info(drl);
                // Create session
                kieServices.newKieBuilder(customKfs).buildAll();
                customKcontainer = kieServices.newKieContainer(kieServices.getRepository().getDefaultReleaseId());
            } else {
                customKcontainer = null;
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

        InputStream templateStreamV = this.getClass().getResourceAsStream(CUSTOM_VICINITY_RULE_TEMPLATE);
        TemplateContainer tcV = new DefaultTemplateContainer(templateStreamV);
        TemplateDataListener listenerV = new TemplateDataListener(tcV);

        int rowNum = 0;
        int rowNumV = 0;

        for (CustomRuleDto ruleDto : ruleDtos) {
            if(ruleDto.getExpression().contains("vicinity")){

                String vExpression = vicinityReplacement(ruleDto.getExpression());

                listenerV.newRow(rowNumV, 0);
                listenerV.newCell(rowNumV, 0, ruleDto.getRuleName(), 0);
                listenerV.newCell(rowNumV, 1, vExpression, 0);
                listenerV.newCell(rowNumV, 2, ruleDto.getAction(), 0);
                listenerV.newCell(rowNumV, 3, ruleDto.getRuleGuid(), 0);
                rowNumV++;

            }else {
                listener.newRow(rowNum, 0);
                listener.newCell(rowNum, 0, ruleDto.getRuleName(), 0);
                listener.newCell(rowNum, 1, ruleDto.getExpression(), 0);
                listener.newCell(rowNum, 2, ruleDto.getAction(), 0);
                listener.newCell(rowNum, 3, ruleDto.getRuleGuid(), 0);
                rowNum++;
            }
        }
        listener.finishSheet();
        listenerV.finishSheet();
        String drl = listener.renderDRL() + "\n" + listenerV.renderDRL();

        LOG.debug("Custom rule file:\n{}", drl);

        return drl;
    }

    private String vicinityReplacement(String oldExpression){

        String[] splitExpression = oldExpression.split("(&&)|(\\|\\|)");

        Pattern splitter = Pattern.compile("(&&)|(\\|\\|)");
        Matcher splitterMatcher = splitter.matcher(oldExpression);

        Pattern p = Pattern.compile(valuePattern);
        StringBuilder resultBuilder = new StringBuilder();
        //resultBuilder.append("$movementDetails : MovementDetails()\n");
        for (int i = 0; i < splitExpression.length; i++) {
            if(i == 0){} //do nothing
            else{
                splitterMatcher.find();
                resultBuilder.append( "\n" + splitterMatcher.group() + " ");
            }
            resultBuilder.append("\n");

            if(splitExpression[i].contains("vicinity")){
                Matcher valueMatcher = p.matcher(splitExpression[i]);
                valueMatcher.find();
                String value = valueMatcher.group();

                if(splitExpression[i].contains("Of")) {
                    if(splitExpression[i].charAt(1) == '!'){
                        resultBuilder.append(addVicinityText(" asset != " + value));
                    }else {
                        resultBuilder.append(addVicinityText(" asset == " + value));
                    }
                }else {
                    if(splitExpression[i].charAt(1) == '!') {
                        resultBuilder.append(addVicinityText(" distance != " + value));
                    }else{
                        resultBuilder.append(addVicinityText(" distance == " + value));
                    }
                }
            }else {
                resultBuilder.append(addNormalText(splitExpression[i]));
            }

        }

        return resultBuilder.toString();

    }

    private String addVicinityText(String variableAndValue){
        String s = "(MovementDetails( $vicOf: vicinityOf ) \n VicinityInfoDTO ( " + variableAndValue + ") from $vicOf)";
        return s;
    }

    private String addNormalText(String variableAndValue){
        String s = "$movementDetails : MovementDetails( " + variableAndValue + ")";
        return s;
    }


}