/*
 *
 * Developed by the European Commission - Directorate General for Maritime Affairs and Fisheries © European Union, 2015-2016.
 *
 * This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of
 * the License, or any later version. The IFDM Suite is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 *
 *
 */

package eu.europa.ec.fisheries.uvms.rules.service.bean;

import eu.europa.ec.fisheries.schema.rules.rule.v1.ErrorType;
import eu.europa.ec.fisheries.schema.rules.rule.v1.RawMessageType;
import eu.europa.ec.fisheries.uvms.rules.domain.RulesDomainModel;
import eu.europa.ec.fisheries.uvms.rules.model.dto.ValidationResultDto;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelException;
import eu.europa.ec.fisheries.uvms.rules.service.business.AbstractFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.RuleError;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaReportDocumentFact;
import eu.europa.ec.fisheries.uvms.rules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;

import java.util.Arrays;

import static junit.framework.Assert.*;

/**
 * Created by padhyad on 6/7/2017.
 */
public class RulesPostProcessBeanTest {

    @Rule
    public MockitoRule mockitoRule = MockitoJUnit.rule();

    @InjectMocks
    RulePostProcessBean rulePostProcessBean;

    @Mock
    RulesDomainModel rulesDomainModel;

    @Test
    public void testCheckAndUpdateValidationErrorExist() throws RulesModelException, RulesServiceException {
        Mockito.doNothing().when(rulesDomainModel).saveValidationMessages(Mockito.any(RawMessageType.class));
        AbstractFact fact = new FaReportDocumentFact();
        fact.addWarningOrError("ERROR", "Test Error", "br01", "L00", "null");
        fact.addWarningOrError("WARNING", "Test Warning", "br02", "L01", "null");
        fact.setOk(false);

        ValidationResultDto validationResult = rulePostProcessBean.checkAndUpdateValidationResult(Arrays.asList(fact), "<FLUXFAReportMessage></FLUXFAReportMessage>");
        assertTrue(validationResult.isError());
        assertTrue(validationResult.isWarning());
        assertFalse(validationResult.isOk());
        assertEquals(2, validationResult.getValidationMessages().size());
    }

    @Test
    public void testCheckAndUpdateValidationErrorNotExist() throws RulesModelException, RulesServiceException {
        Mockito.doNothing().when(rulesDomainModel).saveValidationMessages(Mockito.any(RawMessageType.class));
        AbstractFact fact = new FaReportDocumentFact();
        fact.setOk(true);

        ValidationResultDto validationResult = rulePostProcessBean.checkAndUpdateValidationResult(Arrays.asList(fact), "<FLUXFAReportMessage></FLUXFAReportMessage>");
        assertFalse(validationResult.isError());
        assertFalse(validationResult.isWarning());
        assertTrue(validationResult.isOk());
    }



    @Test
    public void testCheckAndUpdateValidationResultForGeneralBuinessRules() throws RulesModelException, RulesServiceException {
        Mockito.doNothing().when(rulesDomainModel).saveValidationMessages(Mockito.any(RawMessageType.class));
        AbstractFact fact = new FaReportDocumentFact();
        fact.setOk(true);
        RuleError ruleError= new RuleError(ServiceConstants.INVALID_XML_RULE,  ServiceConstants.INVALID_XML_RULE_MESSAGE,  "L00", null);;
        ValidationResultDto validationResult = rulePostProcessBean.checkAndUpdateValidationResultForGeneralBuinessRules(ruleError, ErrorType.ERROR, "<FLUXFAReportMessage></FLUXFAReportMessage>");
        assertFalse(validationResult.isError());
        assertFalse(validationResult.isWarning());
        assertFalse(validationResult.isOk());
    }

}
