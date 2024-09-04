package namedEntities.heuristics;

import java.text.Normalizer;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AllWordsHeuristic implements NamedEntityHeuristic {

    @Override
    public List<String> extractCandidates(String text) {
        List<String> candidates = new ArrayList<>();

        // Normalizar el texto y eliminar caracteres no deseados
        text = text.replaceAll("[-+.^:,\"]", "");
        text = Normalizer.normalize(text, Normalizer.Form.NFD);
        text = text.replaceAll("\\p{M}", "");

        // Patrón para encontrar todas las palabras
        Pattern pattern = Pattern.compile("\\b\\w+\\b");

        Matcher matcher = pattern.matcher(text);

        // Agregar todas las palabras encontradas a la lista de candidatos
        while (matcher.find()) {
            candidates.add(matcher.group());
        }
        return candidates;
    }
}
