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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import java.util.TimeZone;

public class RulesUtil {
    
    private static final String FORMAT = "yyyy-MM-dd HH:mm:ss Z";

    private RulesUtil() {}
    
    public static Date stringToDate(String dateString) throws ParseException {
        if (dateString != null) {
            DateFormat df = new SimpleDateFormat(FORMAT);
            df.parse(dateString);

            return df.parse(dateString);
        } else {
            return null;
        }
    }

    public static String dateToString(Date date) {
        String dateString = null;
        if (date != null) {
            DateFormat df = new SimpleDateFormat(FORMAT);
            df.setTimeZone(TimeZone.getTimeZone("UTC"));
            dateString = df.format(date);
        }
        return dateString;
    }

}