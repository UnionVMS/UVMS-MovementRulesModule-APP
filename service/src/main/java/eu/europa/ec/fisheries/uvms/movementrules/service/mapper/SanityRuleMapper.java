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
package eu.europa.ec.fisheries.uvms.movementrules.service.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SanityRuleType;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.SanityRule;

public class SanityRuleMapper {
    
    private static final Logger LOG = LoggerFactory.getLogger(SanityRuleMapper.class);

    private SanityRuleMapper() {}

    public static SanityRuleType toSanityRuleType(SanityRuleType sanityRuleType, SanityRule sanityRuleEntity) {
        sanityRuleType.setName(sanityRuleEntity.getName());
        sanityRuleType.setGuid(sanityRuleEntity.getGuid());
        sanityRuleType.setExpression(sanityRuleEntity.getExpression());
        sanityRuleType.setDescription(sanityRuleEntity.getDescription());
        sanityRuleType.setUpdated(DateUtils.dateToString(sanityRuleEntity.getUpdated()));
        sanityRuleType.setUpdatedBy(sanityRuleEntity.getUpdatedBy());

        return sanityRuleType;
    }

    public static SanityRule toSanityRuleEntity(SanityRule sanityRuleEntity, SanityRuleType sanityRuleType) {
        Date now = new Date();

        // Base
        sanityRuleEntity.setName(sanityRuleType.getName());
        sanityRuleEntity.setGuid(sanityRuleType.getGuid());
        sanityRuleEntity.setExpression(sanityRuleType.getExpression());
        sanityRuleEntity.setDescription(sanityRuleType.getDescription());
        sanityRuleEntity.setUpdated(now);
        sanityRuleEntity.setUpdatedBy(sanityRuleType.getUpdatedBy());

        return sanityRuleEntity;
    }

    public static SanityRule toSanityRuleEntity(SanityRuleType sanityRuleType) {
        SanityRule sanityRuleEntity = new SanityRule();
        return toSanityRuleEntity(sanityRuleEntity, sanityRuleType);
    }

    public static SanityRuleType toSanityRuleType(SanityRule sanityRuleEntity) {
        SanityRuleType sanityRuleType = new SanityRuleType();
        return toSanityRuleType(sanityRuleType, sanityRuleEntity);
    }
    
    public static List<SanityRuleType> toSanityRuleTypeList(List<SanityRule> sanityRules) {
        List<SanityRuleType> sanityRuleTypes = new ArrayList<>();
        for (SanityRule sanityRule : sanityRules) {
            sanityRuleTypes.add(toSanityRuleType(sanityRule));
        }
        return sanityRuleTypes;
    }
}