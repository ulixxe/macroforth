install_dir = /usr/lib/gforth/site-forth/macroforth
doc_install_dir = /usr/local/share/doc/macroforth

fs_sources = cross.fs asm.fs prims.fs kernel.fs
doc_files = README.txt COPYING

all:

install: $(fs_sources) $(doc_files)
	install -d $(install_dir) $(doc_install_dir)
	install -t $(install_dir) -m 644 $(fs_sources)
	install -t $(doc_install_dir) -m 644 $(doc_files)
