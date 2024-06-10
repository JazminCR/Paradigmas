package namedEntities.heuristics;

import java.util.List;

public interface NamedEntityHeuristic {

    List<String> extractCandidates(String text);
}