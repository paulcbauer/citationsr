# citations
The R package 'citations' comprises functions that can be used to analyze the 'quality' of citations.

The functions contained should help to 

1. Save data on studies whose citations we want to investigate: study_data()
2. Scrape a study's citations: get_wos_citations()
3. Scrape the fulltext (PDFs) of those citations: gen_ris() and fetch_paperpiledocs()
4. Extract text from those PDFs and save in text files: extract_text()
5. Clean the extracted text documents: clean_text()
6. Identify whether those documents really cite the study of interest: identify_study_in_text()
7. Delete certain sections in texts: delete_reference_section()
8. Extract citations cases from these documents: extract_citation_cases()
9. Clean the extracted citation cases: clean_citation_cases()
10. Classify the citation cases: future...
