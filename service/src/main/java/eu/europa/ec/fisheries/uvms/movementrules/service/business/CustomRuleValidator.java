package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.format.DateTimeParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CustomRuleValidator {
    private static final Logger LOG = LoggerFactory.getLogger(CustomRuleValidator.class);

    private CustomRuleValidator() {
    }

    public static boolean isCustomRuleValid(CustomRuleType customRule) {
        boolean nameValid = isNameValid(customRule);
        boolean descriptionValid = isDescriptionValid(customRule);
        boolean timeIntervalsValid = isTimeIntervalsValid(customRule);
        boolean definitionsValid = isDefinitionsValid(customRule);
        boolean actionsValid = isActionsValid(customRule);

        return nameValid &&
                descriptionValid &&
                timeIntervalsValid &&
                definitionsValid &&
                actionsValid;
    }

    private static boolean isNameValid(CustomRuleType customRule) {
        return customRule.getName() != null && !customRule.getName().isEmpty();
    }

    private static boolean isDescriptionValid(CustomRuleType customRule) {
        return customRule.getDescription() != null && !customRule.getDescription().isEmpty();
    }

    private static boolean isTimeIntervalsValid(CustomRuleType customRule) {
        if(customRule.getTimeIntervals() != null && !customRule.getTimeIntervals().isEmpty()) {
            for (CustomRuleIntervalType intervalType : customRule.getTimeIntervals()) {
                if (intervalType.getStart() == null || intervalType.getStart().isEmpty() ||
                        intervalType.getEnd() == null || intervalType.getEnd().isEmpty()) {
                    return false;
                }
                try {
                    if (MRDateUtils.stringToDate(intervalType.getStart()).isAfter(MRDateUtils.stringToDate(intervalType.getEnd()))) {
                        return false;
                    }
                } catch (DateTimeParseException e) {
                    LOG.error("Error in parsing date, returning non-valid customRule. Error message: " + e.getMessage());
                    return false;
                }
            }
            return true;
        }
        return true;
    }

    private static boolean isDefinitionsValid(CustomRuleType customRule) {
        if(customRule.getDefinitions() == null || customRule.getDefinitions().isEmpty()) {
            return false;
        }
        int startOperators = 0;
        int endOperators = 0;

        for (int i = 0; i < customRule.getDefinitions().size(); i++) {
            CustomRuleSegmentType segment = customRule.getDefinitions().get(i);

            if(!(segment.getStartOperator().startsWith("(") || segment.getStartOperator().isEmpty()) ||
                    !(segment.getEndOperator().startsWith(")") || segment.getEndOperator().isEmpty())){
                return false;
            }
            startOperators += segment.getStartOperator().length();
            endOperators += segment.getEndOperator().length();

            if (LogicOperatorType.NONE.equals(segment.getLogicBoolOperator()) &&
                    i < (customRule.getDefinitions().size() - 1)) {
                return false;
            }
            if (!LogicOperatorType.NONE.equals(segment.getLogicBoolOperator()) &&
                    i == (customRule.getDefinitions().size() - 1)) {
                return false;
            }
        }
        return startOperators == endOperators;
    }

    private static boolean isActionsValid(CustomRuleType customRule) {
        if(customRule.getActions() != null && !customRule.getActions().isEmpty()) {
            for(CustomRuleActionType action : customRule.getActions()) {
                if(action.getAction().equals(ActionType.EMAIL)) {
                    Pattern VALID_EMAIL_REGEX = Pattern.compile(
                            "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE);
                    Matcher matcher = VALID_EMAIL_REGEX.matcher(action.getValue());
                    return matcher.find();
                }
            }
        }
        return true;
    }
}
