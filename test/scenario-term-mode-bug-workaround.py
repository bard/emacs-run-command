import time
import sys

toolbar_width = 40

sys.stdout.write("[%s]" % (" " * toolbar_width))
sys.stdout.flush()
sys.stdout.write("\b" * (toolbar_width+1))

for i in range(toolbar_width):
    time.sleep(0.01)
    sys.stdout.write("-")
    sys.stdout.flush()

sys.stdout.write("]\n")
