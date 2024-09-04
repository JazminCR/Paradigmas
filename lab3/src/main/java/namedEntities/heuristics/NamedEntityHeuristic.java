package namedEntities.heuristics;

import java.util.List;
import java.io.Serializable;

public interface NamedEntityHeuristic extends Serializable {

    List<String> extractCandidates(String text);
}