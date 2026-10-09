# word frequencies with dicts, sorting and f-strings
text = """the quick brown fox jumps over the lazy dog
the dog barks and the fox runs away"""

counts = {}
for word in text.split():
    counts[word] = counts.get(word, 0) + 1

ranked = sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))
for word, n in ranked[:5]:
    print(f"{word:<8}{n:>3}  {'#' * n}")

print("unique words:", len(counts))
print("longest:", max(counts, key=len))
