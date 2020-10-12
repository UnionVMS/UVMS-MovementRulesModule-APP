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
package eu.europa.ec.fisheries.uvms.movementrules.service.constants;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;

public class ServiceConstants {

    // Rule GUID for Asset not sending rule
    public static final String ASSET_NOT_SENDING_RULE = "Asset not sending";

    public static final CustomRule ASSET_NOT_SENDING_CUSTOMRULE = new CustomRule();

    static {
        ASSET_NOT_SENDING_CUSTOMRULE.setAvailability(AvailabilityType.GLOBAL);
        RuleSubscription notSendingSubscription = new RuleSubscription();
        notSendingSubscription.setOwner(ServiceConstants.ASSET_NOT_SENDING_RULE);
        ASSET_NOT_SENDING_CUSTOMRULE.getRuleSubscriptionList().add(notSendingSubscription);

    }

}