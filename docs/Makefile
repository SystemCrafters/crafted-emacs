
ORG_FILES=$(wildcard crafted-*.org) getting-started-guide.org mit-license.org

docs: crafted-emacs.info

crafted-emacs.info: $(ORG_FILES)
	emacs -Q --script crafted-docs.el --funcall crafted-docs-export-info
