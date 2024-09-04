package namedEntities.heuristics;

import java.text.Normalizer;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AcronymsHeuristic implements NamedEntityHeuristic {

    @Override
    public List<String> extractCandidates(String text) {
        List<String> candidates = new ArrayList<>();

        text = text.replaceAll("[-+.^:,\"]", "");
        text = Normalizer.normalize(text, Normalizer.Form.NFD);
        text = text.replaceAll("\\p{M}", "");

        Pattern acronymPattern = Pattern.compile("\\b[A-Z]{2,}\\b");

        Matcher matcher = acronymPattern.matcher(text);

        while (matcher.find()) {
            candidates.add(matcher.group());
        }
        return candidates;
    }
}