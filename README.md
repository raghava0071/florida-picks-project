


## Next “top-1%” steps (after README)

These make your project look and behave like a serious data service:

1. **Release the dataset weekly**
   ```bash
   # tag a weekly release when data changes
   git tag -a "data-$(date +%Y-%m-%d)" -m "Weekly data snapshot"
   git push --tags

## Automation
![update-data](https://github.com/raghava0071/florida-picks-project/actions/workflows/update.yml/badge.svg)
