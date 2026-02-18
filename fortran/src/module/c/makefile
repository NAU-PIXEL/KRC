CMODDIR := $(PWD)
CMODSOURCES := $(wildcard $(CMODDIR)/*.c)
CMODHEADERS := $(wildcard $(CMODDIR)/*.h)
CMODOBJS := $(subst .c,.o,$(CMODSOURCES))

$(CMODOBJS): $(CMODDIR)/%.o: $(CMODDIR)/%.c $(CMODHEADERS)
	gcc -g -c $< -o $@

main: $(CMODOBJS)
	gcc $(CMODOBJS) -o main