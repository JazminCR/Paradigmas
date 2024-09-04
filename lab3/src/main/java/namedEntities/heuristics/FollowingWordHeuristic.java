package namedEntities.heuristics;

import java.text.Normalizer;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FollowingWordHeuristic implements NamedEntityHeuristic {

    @Override
    public List<String> extractCandidates(String text) {
        List<String> candidates = new ArrayList<>();

        text = text.replaceAll("[-+.^:,\"]", "");
        text = Normalizer.normalize(text, Normalizer.Form.NFD);
        text = text.replaceAll("\\p{M}", "");

        String[] words = text.split("\\s+");

        List<String> keyWords = Arrays.asList("en", "de", "sr.", "sra.");

        for (int i = 0; i < words.length - 1; i++) {
            String currentWord = words[i].replaceAll("[^a-zA-Z0-9]", "");
            if (keyWords.contains(currentWord.toLowerCase())) {
                String nextWord = words[i + 1].replaceAll("^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$", "");
                candidates.add(words[i + 1]);
            }
        }

        return candidates;
    }
}
