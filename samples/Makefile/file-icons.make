charmap     := charmap.md
font-name   := file-icons
font-folder := dist
font-config := icomoon.json
icon-size   := 34
icon-folder := svg
repo-name   := Alhadis/FileIcons
svg         := $(wildcard $(icon-folder)/*.svg)
last-commit  = $(shell git log -1 --oneline --no-abbrev | cut -d' ' -f1)


all: unpack $(font-folder)/$(font-name).woff2 charmap


# Aliases
unpack:  $(font-folder)/$(font-name).ttf
charmap: $(charmap)


# Extract a downloaded IcoMoon folder
$(font-folder)/%.ttf: %.zip
	@rm -rf $(font-folder) tmp $(font-config)
	@unzip -qd tmp $^
	@mv tmp/fonts $(font-folder)
	@mv tmp/selection.json $(font-config)
	@rm -rf tmp $^
	@perl -pi -e 's|^( {2})+|"\t" x (length($$&)/2)|ge' $(font-config)
	@echo "" >> $(font-config) # Ensure trailing newline
	@echo "Files extracted."


# Generate a WOFF2 file from a TTF
%.woff2: %.ttf
	@[ ! -f $@ ] && { \
		hash woff2_compress 2>/dev/null || { \
			echo >&2 "WOFF2 conversion tools not found. Consult the readme file."; \
			exit 2; \
		}; \
		woff2_compress $^ >/dev/null; \
		echo "WOFF2 file generated."; \
	};
	


# Clean up SVG source
lint: $(svg)
	@perl -0777 -pi -e '\
		s/\r\n/\n/g; \
		s/<g id="icomoon-ignore">\s*<\/g>//gmi; \
		s/<g\s*>\s*<\/g>//gmi; \
		s/\s+(id|viewBox|xml:space)="[^"]*"/ /gmi; \
		s/<!DOCTYPE[^>]*>//gi; \
		s/<\?xml.*?\?>//gi; \
		s/<!--.*?-->//gm; \
		s/ style="enable-background:.*?;"//gmi; \
		s/"\s+>/">/g; \
		s/\x20{2,}/ /g; \
		s/[\t\n]+//gm;' $^



# Generate/update character map
$(charmap):
	@./create-map.pl -r=$(repo-name) -i=$(icon-folder) --size=$(icon-size) $(font-folder)/$(font-name).svg $@




# POSIX systems only: reattach hard links to File-Icons package
relink:
	@$(call need-var,ATOM_FILE_ICONS,ERROR_NO_PKG)
	@ln -f $(font-folder)/$(font-name).woff2 $(wildcard $(ATOM_FILE_ICONS)/fonts/file-icons-*.woff2)



# Force an icon's preview to be refreshed on GitHub
cachebust:
	@$(call need-var,icon,ERROR_NO_ICON)
	@base="https://cdn.rawgit.com/Alhadis/FileIcons/"; \
	perl -pi -e 's{$$base\K\w+(?=/svg/$(icon:%.svg=%)\.svg")}{$(last-commit)}ig;' $(charmap)


# Dummy task to improve feedback if `cachebust` is mistyped
icon:
	$(call need-var,,ERROR_UNDEF_ICON)



# Reset unstaged changes/additions in object directories
clean:
	@git clean -fd $(font-folder)
	@git checkout -- $(font-folder) 2>/dev/null || true


# Delete extracted and generated files
distclean:
	@rm -rf $(font-folder)


.PHONY: clean distclean $(charmap) cachebust icon
.ONESHELL:


# Error message shown to users attempting to run `make relink` without a link
ERROR_NO_PKG := Environment variable ATOM_FILE_ICONS not found. \
	| \
	| Try this instead:\
	| \
	| \	make relink ATOM_FILE_ICONS=/path/to/your/file-icons/installation | 


# Error message shown when running `make cachebust` without an icon
ERROR_NO_ICON := No icon specified. Task aborted.| \
	| Usage: \
	| \	make icon=file[.svg] cachebust \
	| \
	| Examples: \
	| \	make icon=Manpage cachebust \
	| \	make icon=APL.svg cachebust | 


# Shown if user tries running `make icon NAME cachebust` by mistake
ERROR_UNDEF_ICON := No task named \"icon\". \
	| \
	| Did you mean this? \
	| \	make icon=NAME cachebust | 
	


# If the given value is empty, die with an error message
need = @$(if $(1),,echo $(subst | ,$$'\n',$(2)); exit 2)

# Like `need`, but uses variable names instead of string values
need-var = @$(call need,$($(1)),$($(2)))
