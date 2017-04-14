package net.grinder.statistics;

public class StatisticsAccessor {
    public static StatisticsSetFactory getFactory() {
        StatisticsIndexMap statisticsIndexMap=new StatisticsIndexMap();
        return new StatisticsSetFactory(statisticsIndexMap);
    }
}
