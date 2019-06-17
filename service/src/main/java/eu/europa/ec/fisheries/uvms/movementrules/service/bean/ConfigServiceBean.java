package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalStatus;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.MainCriteria;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.SubCriteria;

import javax.ejb.Stateless;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Stateless
public class ConfigServiceBean {

    public Map<String, HashMap<String, ArrayList<String>>> getCriterias() {
        Map<String, HashMap<String, ArrayList<String>>> map = new HashMap<>();
        MainCriteria[] mainCriterias = MainCriteria.values();
        for (MainCriteria mainCriteria : mainCriterias) {
            HashMap<String, ArrayList<String>> subResult = new HashMap<>();
            SubCriteria[] subCriterias = SubCriteria.values();
            for (SubCriteria subCriteria : subCriterias) {
                if (subCriteria.getMainCriteria().equals(mainCriteria)) {
                    subResult.put(subCriteria.toString(), getConditionsByCriteria(subCriteria));
                }
                if (!mainCriteria.equals(MainCriteria.ROOT)) {
                    map.put(mainCriteria.name(), subResult);
                }
            }
        }
        return map;
    }

    public LogicOperatorType[] getLogicOperatorType() {
        return LogicOperatorType.values();
    }

    public AvailabilityType[] getAvailability() {
        return AvailabilityType.values();
    }

    public MobileTerminalStatus[] getMobileTerminalStatuses() {
        return MobileTerminalStatus.values();
    }

    public AssetStatus[] getAssetStatuses() {
        return AssetStatus.values();
    }


    public Map getActions() {
        Map map = new HashMap();
        // NeedValue is true for all ActionTypes. Update if new ActionTypes with a false value are added.
        Arrays.stream(ActionType.values()).forEach(actionType -> map.put(actionType, true));
        return map;
    }

    private ArrayList<String> getConditionsByCriteria(SubCriteria subCriteria) {
        ArrayList<String> conditions = new ArrayList<>();
        switch (subCriteria) {
            case ACTIVITY_CALLBACK:
            case ACTIVITY_MESSAGE_ID:
            case ACTIVITY_MESSAGE_TYPE:
            case AREA_CODE:
            case AREA_TYPE:
            case ASSET_ID_GEAR_TYPE:
            case EXTERNAL_MARKING:
            case ASSET_NAME:
            case COMCHANNEL_TYPE:
            case MT_TYPE:
            case FLAG_STATE:
            case MOVEMENT_TYPE:
            case SEGMENT_TYPE:
            case SOURCE:
            case CLOSEST_COUNTRY_CODE:
            case CLOSEST_PORT_CODE:
            case ASSET_GROUP:
            case ASSET_STATUS:
            case MT_STATUS:
            case AREA_CODE_ENT:
            case AREA_TYPE_ENT:
            case AREA_CODE_EXT:
            case AREA_TYPE_EXT:
            case VICINITY_OF:
                conditions.add(ConditionType.EQ.name());
                conditions.add(ConditionType.NE.name());
                break;

            case ASSET_CFR:
            case ASSET_IRCS:
            case MT_DNID:
            case MT_MEMBER_ID:
            case MT_SERIAL_NO:
            case ALTITUDE:
            case LATITUDE:
            case LONGITUDE:
            case POSITION_REPORT_TIME:
            case STATUS_CODE:
            case REPORTED_COURSE:
            case REPORTED_SPEED:
            case CALCULATED_COURSE:
            case CALCULATED_SPEED:
            case TIME_DIFF_POSITION_REPORT:
            case SUM_POSITION_REPORT:
            default:
                conditions.add(ConditionType.EQ.name());
                conditions.add(ConditionType.NE.name());
                conditions.add(ConditionType.LT.name());
                conditions.add(ConditionType.GT.name());
                conditions.add(ConditionType.LE.name());
                conditions.add(ConditionType.GE.name());
                break;
        }
        return conditions;
    }

}
