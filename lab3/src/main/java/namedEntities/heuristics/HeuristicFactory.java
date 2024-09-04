package namedEntities.heuristics;

import java.util.HashMap;
import java.util.Map;

public class HeuristicFactory {
    private static final Map<String, Class<? extends NamedEntityHeuristic>> heuristics = new HashMap<>();

    static {
        heuristics.put("AcronymsHeuristic", AcronymsHeuristic.class);
        heuristics.put("CapitalizedWordHeuristic", CapitalizedWordHeuristic.class);
        heuristics.put("AllWordsHeuristic", AllWordsHeuristic.class);
        heuristics.put("FollowingWordHeuristic", FollowingWordHeuristic.class);
    }

    public static NamedEntityHeuristic getHeuristic(String key) {
        Class<? extends NamedEntityHeuristic> heuristicClass = heuristics.get(key);
        if (heuristicClass != null) {
            try {
                return heuristicClass.getDeclaredConstructor().newInstance();
            } catch (Exception e) {
                return null;
            }
        } else {
            return null;
        }
    }
}
