# Quickstart — Validating the MOB-53697 spike findings (taurus)

The spike ships no code. To validate the findings an engineer can:

1. **Q1/Q4 (iteration ownership)** — read `bzt/modules/_apiritif/executor.py:141-147` and `:186-207` to confirm iterations are passed to the external apiritif.loadgen subprocess.
2. **Q2 (skip point)** — read `bzt/modules/_apiritif/generator.py:2020-2046` (`_gen_master_test_method`) and `:1933-2018` to confirm the generated code is where step sequencing / try-except lives.
3. **Q3 (SKIPPED status)** — read `bzt/modules/functional.py:166` (`TEST_STATUSES`), `:264-265` (load reader drops SKIPPED), `:359-365` (func reader keeps it).
4. **Residual risk** — run `python -m apiritif.loadgen --iterations 2 <script>` with iteration 1 raising an exception; observe whether iteration 2 runs. This is the recommended validation step for the follow-up Story.
