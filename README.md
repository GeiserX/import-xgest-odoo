<p align="center"><img src="docs/images/banner.svg" alt="import-xgest-odoo" width="900"/></p>

<h1 align="center">import-xgest-odoo</h1>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/github/license/GeiserX/import-xgest-odoo" alt="License"/></a>
</p>

<p align="center"><strong>Migration scripts between Xgest and Odoo ERPs</strong></p>

---

R and Python scripts that export customers, suppliers, articles and helpdesk tickets from the Xgest ERP (MySQL) as CSV files ready for Odoo.

## Quick start

Each R script reads one kind of Xgest record (customers, suppliers, or articles and stock) and writes it as CSV for Odoo. You need R with `RMySQL`, `BBmisc` and `stringr`, and MySQL access to the Xgest database. Set the `dbConnect()` host, user and password at the top of the script you need and the output path at its end, then:

```bash
Rscript ClientesSAF14.R
```

[`calcIBAN.py`](calcIBAN.py) (Python 2) computes Spanish IBANs; [`tickets/importTickets.R`](tickets/importTickets.R) converts a Web Help Desk ticket export.

## License

[GPL-3.0-or-later](LICENSE)
