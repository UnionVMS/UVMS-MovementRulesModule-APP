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
package eu.europa.ec.fisheries.uvms.rules.rest.service;

import java.util.List;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import eu.europa.ec.fisheries.uvms.rules.entity.SanityRule;
import eu.europa.ec.fisheries.uvms.rules.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.rules.mapper.SanityRuleMapper;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.rules.rest.dto.ResponseCode;
import eu.europa.ec.fisheries.uvms.rules.rest.dto.ResponseDto;
import eu.europa.ec.fisheries.uvms.rules.rest.error.ErrorHandler;
import eu.europa.ec.fisheries.uvms.rules.service.ValidationService;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;

@Path("/sanityrules")
@Stateless
@RequiresFeature(UnionVMSFeature.viewAlarmRules)
public class SanityRulesRestResource {

    private static final Logger LOG = LoggerFactory.getLogger(SanityRulesRestResource.class);

    @EJB
    ValidationService validationService;

    /**
     *
     * @responseMessage 200 [Success]
     * @responseMessage 500 [Error]
     *
     * @summary Get a list of all sanity rules
     *
     */
    @GET
    @Consumes(value = { MediaType.APPLICATION_JSON })
    @Produces(value = { MediaType.APPLICATION_JSON })
    @Path(value = "listAll")
    public ResponseDto getSanityRules() {
        try {
            List<SanityRule> sanityRules = validationService.getSanityRules();
            return new ResponseDto(SanityRuleMapper.toSanityRuleTypeList(sanityRules), ResponseCode.OK);
        } catch (RulesServiceException | RulesFaultException | NullPointerException | DaoMappingException ex) {
            LOG.error("[ Error when getting all sanity rules. ] {} ", ex);
            return ErrorHandler.getFault(ex);
        }
    }

}