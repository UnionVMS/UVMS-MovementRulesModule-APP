package eu.europa.ec.fisheries.uvms.movementrules.model;

import com.fasterxml.jackson.datatype.jsr310.deser.InstantDeserializer;

import java.time.format.DateTimeFormatter;

public class MovementRulesInstantDeserializer extends InstantDeserializer {
    public MovementRulesInstantDeserializer(){
        super(InstantDeserializer.INSTANT ,DateTimeFormatter.ISO_INSTANT);
    }
}
